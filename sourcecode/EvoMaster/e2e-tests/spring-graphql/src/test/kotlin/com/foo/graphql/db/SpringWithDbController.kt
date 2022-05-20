package com.foo.graphql.db

import com.foo.graphql.SpringController
import org.evomaster.client.java.controller.api.dto.database.schema.DatabaseType
import org.evomaster.client.java.controller.internal.db.DbSpecification
import org.hibernate.dialect.H2Dialect
import org.springframework.boot.SpringApplication
import org.springframework.jdbc.core.JdbcTemplate
import java.sql.Connection
import java.sql.SQLException
import java.util.*


abstract class SpringWithDbController(applicationClass: Class<*>) : SpringController(applicationClass) {

    companion object{
        init {
            /**
             * To avoid issues with non-determinism checks (in particular in the handling of taint-analysis),
             * we must disable the cache in H2
             */
            System.setProperty("h2.objectCache", "false")
        }

        private var dbID = 0
    }

    var dbconnection : Connection? = null


    override fun startSut(): String {
        //lot of problem if using same H2 instance. see:
        //https://github.com/h2database/h2database/issues/227
        val rand = dbID++ //nextInt()

        ctx = SpringApplication.run(applicationClass,
            "--server.port=0",
            "--graphql.tools.schema-location-pattern=**/${schemaName()}",
            "--spring.datasource.url=jdbc:h2:mem:testdb_"+rand+";DB_CLOSE_DELAY=-1;",
            "--spring.jpa.database-platform=" + H2Dialect::class.java.name,
            "--spring.datasource.username=sa",
            "--spring.datasource.password",
            "--spring.jpa.properties.hibernate.show_sql=true"
        )

        if (dbconnection != null){
            try {
                dbconnection!!.close()
            }catch (e : SQLException){
                e.printStackTrace()
            }
        }
        ctx?:throw IllegalStateException("fail to initialize ctx")
        val jdbc = ctx!!.getBean(JdbcTemplate::class.java)

        try {
            dbconnection = jdbc.dataSource.connection
        }catch (e : SQLException){
            throw RuntimeException(e)
        }

        return "http://localhost:$sutPort"
    }


    override fun resetStateOfSUT() {
//        if (dbconnection != null)
//            DbCleaner.clearDatabase_H2(dbconnection)
    }

    override fun stopSut() {
        super.stopSut()
//        dbconnection = null
    }

    override fun getDbSpecifications(): MutableList<DbSpecification>? =mutableListOf(DbSpecification()
        .apply {
            connection = dbconnection
            dbType = DatabaseType.H2
        })

    override fun getDatabaseDriverName() = "org.h2.Driver"
}