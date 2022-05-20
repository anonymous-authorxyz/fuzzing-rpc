package org.evomaster.core.database

import org.evomaster.client.java.controller.api.dto.database.schema.DatabaseType
import org.evomaster.core.database.schema.Column
import org.evomaster.core.database.schema.ColumnDataType
import org.evomaster.core.database.schema.ForeignKey
import org.evomaster.core.database.schema.Table
import org.evomaster.core.parser.RegexHandler.createGeneForPostgresLike
import org.evomaster.core.parser.RegexHandler.createGeneForPostgresSimilarTo
import org.evomaster.core.search.gene.*
import org.evomaster.core.search.gene.datetime.DateGene
import org.evomaster.core.search.gene.datetime.DateTimeGene
import org.evomaster.core.search.gene.sql.time.SqlTimeIntervalGene
import org.evomaster.core.search.gene.datetime.TimeGene
import org.evomaster.core.search.gene.sql.geometric.*
import org.evomaster.core.search.gene.sql.network.SqlCidrGene
import org.evomaster.core.search.gene.sql.network.SqlInetGene
import org.evomaster.core.search.gene.sql.network.SqlMacAddrGene
import org.evomaster.core.search.gene.regex.DisjunctionListRxGene
import org.evomaster.core.search.gene.regex.RegexGene
import org.evomaster.core.search.gene.sql.*
import org.evomaster.core.search.gene.sql.textsearch.SqlTextSearchQueryGene
import org.evomaster.core.search.gene.sql.textsearch.SqlTextSearchVectorGene
import org.evomaster.core.utils.NumberCalculationUtil
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.math.BigDecimal

class DbActionGeneBuilder {


    private fun getForeignKey(table: Table, column: Column): ForeignKey? {

        //TODO: what if a column is part of more than 1 FK? is that even possible?

        return table.foreignKeys.find { it.sourceColumns.contains(column) }
    }

    fun buildGene(id: Long, table: Table, column: Column): Gene {

        val fk = getForeignKey(table, column)

        var gene = when {

            //TODO handle all constraints and cases
            column.autoIncrement ->
                SqlAutoIncrementGene(column.name)
            fk != null ->
                SqlForeignKeyGene(column.name, id, fk.targetTable, column.nullable)

            else -> when (column.type) {
                // Man: TODO need to check
                ColumnDataType.BIT ->
                    handleBitColumn(column)

                ColumnDataType.VARBIT ->
                    handleBitVaryingColumn(column)

                /**
                 * BOOLEAN(1) is assumed to be a boolean/Boolean field
                 */
                ColumnDataType.BOOLEAN, ColumnDataType.BOOL ->
                    handleBooleanColumn(column)

                /**
                 * TINYINT(3) is assumed to be representing a byte/Byte field
                 */
//                ColumnDataType.TINYINT ->
//                    handleTinyIntColumn(column)

                /**
                 * SMALLINT(5) is assumed as a short/Short field
                 */
//                ColumnDataType.INT2, ColumnDataType.SMALLINT ->
//                    handleSmallIntColumn(column)

                /**
                 * CHAR(255) is assumed to be a char/Character field.
                 * A StringGene of length 1 is used to represent the data.
                 *
                 */
                ColumnDataType.CHAR ->
                    handleCharColumn(column)

                /**
                 * TINYINT(3) is assumed to be representing a byte/Byte field
                 * INT2/SMALLINT(5) is assumed as a short/Short field
                 * INT4/INTEGER(10) is a int/Integer field
                 */
                ColumnDataType.TINYINT, ColumnDataType.INT2, ColumnDataType.SMALLINT, ColumnDataType.INT, ColumnDataType.INT4, ColumnDataType.INTEGER, ColumnDataType.SERIAL, ColumnDataType.MEDIUMINT ->
                    handleIntegerColumn(column)

                /**
                 * BIGINT(19) is a long/Long field
                 */
                ColumnDataType.INT8, ColumnDataType.BIGINT, ColumnDataType.BIGSERIAL ->
                    handleBigIntColumn(column)

                /**
                 * DOUBLE(17) is assumed to be a double/Double field
                 *
                 */

                ColumnDataType.DOUBLE ->
                    handleDoubleColumn(column)

                /**
                 * VARCHAR(N) is assumed to be a String with a maximum length of N.
                 * N could be as large as Integer.MAX_VALUE
                 */
                ColumnDataType.TINYTEXT,
                ColumnDataType.TEXT,
                ColumnDataType.LONGTEXT,
                ColumnDataType.VARCHAR,
                ColumnDataType.CLOB,
                ColumnDataType.MEDIUMTEXT,
                ColumnDataType.LONGBLOB,
                ColumnDataType.MEDIUMBLOB,
                ColumnDataType.TINYBLOB ->
                    handleTextColumn(column)

                //TODO normal TIME, and add tests for it. this is just a quick workaround for patio-api
                ColumnDataType.TIMETZ, ColumnDataType.TIME ->
                    TimeGene(column.name)


                /**
                 * TIMESTAMP is assumed to be a Date field
                 */
                ColumnDataType.TIMESTAMP, ColumnDataType.TIMESTAMPTZ ->
                    handleTimestampColumn(column)

                /**
                 * DATE is a date without time of day
                 */
                ColumnDataType.DATE ->
                    DateGene(column.name)

                /**
                 * TODO need to check with Andrea regarding fsp which is the fractional seconds precision
                 *
                 * see https://dev.mysql.com/doc/refman/8.0/en/date-and-time-type-syntax.html
                 */
                ColumnDataType.DATETIME ->
                    DateTimeGene(column.name)


                ColumnDataType.YEAR ->
                    handleYearColumn(column)

                //column.type.equals("VARBINARY", ignoreCase = true) ->
                //handleVarBinary(it)

                /**
                 * Could be any kind of binary data... so let's just use a string,
                 * which also simplifies when needing generate the test files
                 */
                ColumnDataType.BLOB ->
                    handleBLOBColumn(column)

                ColumnDataType.BYTEA ->
                    handleBinaryStringColumn(column)


                ColumnDataType.REAL,
                ColumnDataType.FLOAT4 ->
                    handleFloatColumn(column, MIN_FLOAT4_VALUE, MAX_FLOAT4_VALUE)

                ColumnDataType.FLOAT8 ->
                    handleFloatColumn(column, MIN_FLOAT8_VALUE, MAX_FLOAT8_VALUE)

                ColumnDataType.DEC,
                ColumnDataType.DECIMAL,
                ColumnDataType.NUMERIC ->
                    handleDecimalColumn(column)

                /**
                 * Postgres UUID column type
                 */
                ColumnDataType.UUID ->
                    SqlUUIDGene(column.name)

                /**
                 * Postgres JSONB column type
                 */
                ColumnDataType.JSON, ColumnDataType.JSONB ->
                    SqlJSONGene(column.name)

                /**
                 * Postgres XML column type
                 */
                ColumnDataType.XML ->
                    SqlXMLGene(column.name)

                ColumnDataType.ENUM ->
                    handleEnumColumn(column)

                ColumnDataType.MONEY ->
                    handleMoneyColumn(column)

                ColumnDataType.BPCHAR ->
                    handleTextColumn(column, isFixedLength = true)

                ColumnDataType.INTERVAL ->
                    SqlTimeIntervalGene(column.name)

                ColumnDataType.POINT ->
                    SqlPointGene(column.name)

                ColumnDataType.LINE ->
                    SqlLineGene(column.name)

                ColumnDataType.LSEG ->
                    SqlLineSegmentGene(column.name)

                ColumnDataType.BOX ->
                    SqlBoxGene(column.name)

                ColumnDataType.PATH ->
                    SqlPathGene(column.name)

                ColumnDataType.POLYGON ->
                    SqlPolygonGene(column.name)

                ColumnDataType.CIRCLE ->
                    SqlCircleGene(column.name)

                ColumnDataType.CIDR ->
                    SqlCidrGene(column.name)

                ColumnDataType.INET ->
                    SqlInetGene(column.name)

                ColumnDataType.MACADDR ->
                    SqlMacAddrGene(column.name)

                ColumnDataType.MACADDR8 ->
                    SqlMacAddrGene(column.name, numberOfOctets = SqlMacAddrGene.MACADDR8_SIZE)

                ColumnDataType.TSVECTOR ->
                    SqlTextSearchVectorGene(column.name)

                ColumnDataType.TSQUERY ->
                    SqlTextSearchQueryGene(column.name)

                ColumnDataType.JSONPATH ->
                    SqlJSONPathGene(column.name)

                ColumnDataType.INT4RANGE ->
                    buildSqlIntegerRangeGene(column)

                ColumnDataType.INT8RANGE ->
                    buildSqlLongRangeGene(column)

                ColumnDataType.NUMRANGE ->
                    buildSqlFloatRangeGene(column)

                ColumnDataType.DATERANGE ->
                    buildSqlDateRangeGene(column)

                ColumnDataType.TSRANGE, ColumnDataType.TSTZRANGE ->
                    buildSqlTimestampRangeGene(column)

                ColumnDataType.INT4MULTIRANGE ->
                    SqlMultiRangeGene(column.name, template = buildSqlIntegerRangeGene(column))

                ColumnDataType.INT8MULTIRANGE ->
                    SqlMultiRangeGene(column.name, template = buildSqlLongRangeGene(column))

                ColumnDataType.NUMMULTIRANGE ->
                    SqlMultiRangeGene(column.name, template = buildSqlFloatRangeGene(column))

                ColumnDataType.DATEMULTIRANGE ->
                    SqlMultiRangeGene(column.name, template = buildSqlDateRangeGene(column))

                ColumnDataType.TSMULTIRANGE, ColumnDataType.TSTZMULTIRANGE ->
                    SqlMultiRangeGene(column.name, template = buildSqlTimestampRangeGene(column))

                ColumnDataType.PG_LSN ->
                    SqlLogSeqNumber(column.name)

                ColumnDataType.COMPOSITE_TYPE ->
                    handleCompositeColumn(id, table, column)

                ColumnDataType.OID,
                ColumnDataType.REGCLASS,
                ColumnDataType.REGCOLLATION,
                ColumnDataType.REGCONFIG,
                ColumnDataType.REGDICTIONARY,
                ColumnDataType.REGNAMESPACE,
                ColumnDataType.REGOPER,
                ColumnDataType.REGOPERATOR,
                ColumnDataType.REGPROC,
                ColumnDataType.REGPROCEDURE,
                ColumnDataType.REGROLE,
                ColumnDataType.REGTYPE ->
                    handleObjectIdentifierType(column.name)


                else -> throw IllegalArgumentException("Cannot handle: $column.")
            }

        }

        if (column.primaryKey) {
            gene = SqlPrimaryKeyGene(column.name, table.name, gene, id)
        }

        if (column.nullable && fk == null) {
            //FKs handle nullability in their own custom way
            gene = SqlNullable(column.name, gene)
        }

        if (column.dimension > 0) {
            // if the column type is an array, matrix, etc.
            gene = SqlMultidimensionalArrayGene<Gene>(column.name, template = gene, numberOfDimensions = column.dimension)
        }

        return gene
    }

    private fun buildSqlTimestampRangeGene(column: Column) =
            SqlRangeGene(column.name, template = buildSqlTimestampGene("bound"))

    private fun buildSqlDateRangeGene(column: Column) =
            SqlRangeGene(column.name, template = DateGene("bound"))

    private fun buildSqlFloatRangeGene(column: Column) =
            SqlRangeGene(column.name, template = FloatGene("bound"))

    private fun buildSqlLongRangeGene(column: Column) =
            SqlRangeGene(column.name, template = LongGene("bound"))

    private fun buildSqlIntegerRangeGene(column: Column) =
            SqlRangeGene(column.name, template = IntegerGene("bound"))

    /*
        https://dev.mysql.com/doc/refman/8.0/en/year.html
     */
    private fun handleYearColumn(column: Column): Gene {
        // Year(2) is not supported by mysql 8.0
        if (column.size == 2)
            return IntegerGene(column.name, 16, min = 0, max = 99)

        return IntegerGene(column.name, 2016, min = 1901, max = 2155)
    }

    private fun handleEnumColumn(column: Column): Gene {
        return EnumGene(name = column.name, data = column.enumValuesAsStrings ?: listOf())
    }

    private fun handleBigIntColumn(column: Column): Gene {
        return if (column.enumValuesAsStrings != null) {
            checkNotEmpty(column.enumValuesAsStrings)
            EnumGene(column.name, column.enumValuesAsStrings.map { it.toLong() })
        } else {
            /*
                TODO might need to use ULong to handle unsigned long
                https://dev.mysql.com/doc/refman/8.0/en/integer-types.html

                Man: TODO need to check whether to update this with BigIntegerGene
             */
            val min: Long? = if (column.isUnsigned) 0 else null
            LongGene(column.name, min = min)
        }
    }

    private fun handleIntegerColumn(column: Column): Gene {
        return if (column.enumValuesAsStrings != null) {
            checkNotEmpty(column.enumValuesAsStrings)
            EnumGene(column.name, column.enumValuesAsStrings.map { it.toInt() })
        } else {

            if ((column.type == ColumnDataType.INT4
                            || column.type == ColumnDataType.INT
                            || column.type == ColumnDataType.INTEGER) && column.isUnsigned
            ) {
                LongGene(column.name, min = 0L, max = 4294967295L)
            } else {
                val min = when {
                    column.isUnsigned -> 0
                    column.type == ColumnDataType.TINYINT -> Byte.MIN_VALUE.toInt()
                    column.type == ColumnDataType.SMALLINT || column.type == ColumnDataType.INT2 -> Short.MIN_VALUE.toInt()
                    column.type == ColumnDataType.MEDIUMINT -> -8388608
                    else -> Int.MIN_VALUE
                }

                val max = when (column.type) {
                    ColumnDataType.TINYINT -> if (column.isUnsigned) 255 else Byte.MAX_VALUE.toInt()
                    ColumnDataType.SMALLINT, ColumnDataType.INT2 -> if (column.isUnsigned) 65535 else Short.MAX_VALUE.toInt()
                    ColumnDataType.MEDIUMINT -> if (column.isUnsigned) 16777215 else 8388607
                    else -> Int.MAX_VALUE
                }

                IntegerGene(
                        column.name,
                        min = column.lowerBound ?: min,
                        max = column.upperBound ?: max
                )
            }
        }
    }

    private fun handleCharColumn(column: Column): Gene {
        //  TODO How to discover if it is a char or a char[] of 255 elements?
        return if (column.enumValuesAsStrings != null) {
            checkNotEmpty(column.enumValuesAsStrings)
            EnumGene(name = column.name, data = column.enumValuesAsStrings)
        } else {
            StringGene(name = column.name, value = "f", minLength = 0, maxLength = 1)
        }
    }

    private fun handleDoubleColumn(column: Column): Gene {
        // TODO How to discover if the source field is a float/Float field?
        return if (column.enumValuesAsStrings != null) {
            checkNotEmpty(column.enumValuesAsStrings)
            EnumGene(name = column.name, data = column.enumValuesAsStrings.map { it.toDouble() })
        } else {
            DoubleGene(column.name)
        }
    }

    private fun handleBinaryStringColumn(column: Column): Gene {
        return if (column.enumValuesAsStrings != null) {
            checkNotEmpty(column.enumValuesAsStrings)
            EnumGene(name = column.name, data = column.enumValuesAsStrings)
        } else {
            SqlBinaryStringGene(name = column.name)
        }
    }

    private fun handleFloatColumn(column: Column, minValue: Double, maxValue: Double): Gene {
        /**
         * REAL is identical to the floating point statement float(24).
         * TODO How to discover if the source field is a float/Float field?
         */
        return if (column.enumValuesAsStrings != null) {
            checkNotEmpty(column.enumValuesAsStrings)
            EnumGene(name = column.name, data = column.enumValuesAsStrings.map { it.toDouble() })

        } else {
            DoubleGene(column.name, min = minValue, max = maxValue)
        }
    }

    @Deprecated("replaced by handleIntegerColumn, now all numeric types resulting in IntegerGene would by handled in handleIntegerColumn")
    private fun handleSmallIntColumn(column: Column): Gene {
        return if (column.enumValuesAsStrings != null) {
            if (column.enumValuesAsStrings.isEmpty()) {
                throw IllegalArgumentException("the list of enumerated values cannot be empty")
            } else {
                EnumGene(column.name, column.enumValuesAsStrings.map { it.toInt() })
            }
        } else {
            IntegerGene(
                    column.name,
                    min = column.lowerBound ?: Short.MIN_VALUE.toInt(),
                    max = column.upperBound ?: Short.MAX_VALUE.toInt()
            )
        }
    }

    @Deprecated("replaced by handleIntegerColumn, now all numeric types resulting in IntegerGene would by handled in handleIntegerColumn")
    private fun handleTinyIntColumn(column: Column): Gene {
        return if (column.enumValuesAsStrings != null) {
            if (column.enumValuesAsStrings.isEmpty()) {
                throw IllegalArgumentException("the list of enumerated values cannot be empty")
            } else {
                EnumGene(column.name, column.enumValuesAsStrings.map { it.toInt() })
            }
        } else {
            IntegerGene(
                    column.name,
                    min = column.lowerBound ?: Byte.MIN_VALUE.toInt(),
                    max = column.upperBound ?: Byte.MAX_VALUE.toInt()
            )
        }
    }

    private fun handleTextColumn(column: Column, isFixedLength: Boolean = false): Gene {
        return if (column.enumValuesAsStrings != null) {
            if (column.enumValuesAsStrings.isEmpty()) {
                throw IllegalArgumentException("the list of enumerated values cannot be empty")
            } else {
                EnumGene(name = column.name, data = column.enumValuesAsStrings)
            }
        } else {
            if (column.similarToPatterns != null && column.similarToPatterns.isNotEmpty()) {
                val columnName = column.name
                val similarToPatterns: List<String> = column.similarToPatterns
                buildSimilarToRegexGene(columnName, similarToPatterns, databaseType = column.databaseType)
            } else if (column.likePatterns != null && column.likePatterns.isNotEmpty()) {
                val columnName = column.name
                val likePatterns = column.likePatterns
                buildLikeRegexGene(columnName, likePatterns, databaseType = column.databaseType)
            } else {
                val columnMinLength = if (isFixedLength) {
                    column.size
                } else {
                    0
                }
                StringGene(name = column.name, minLength = columnMinLength, maxLength = column.size)
            }
        }
    }

    /**
     * Builds a RegexGene using a name and a list of LIKE patterns.
     * The resulting gene is a disjunction of the given patterns
     */
    fun buildLikeRegexGene(geneName: String, likePatterns: List<String>, databaseType: DatabaseType): RegexGene {
        return when (databaseType) {
            DatabaseType.POSTGRES, DatabaseType.MYSQL -> buildPostgresMySQLLikeRegexGene(geneName, likePatterns)
            //TODO: support other database SIMILAR_TO check expressions
            else -> throw UnsupportedOperationException(
                    "Must implement LIKE expressions for database %s".format(
                            databaseType
                    )
            )
        }
    }

    private fun buildPostgresMySQLLikeRegexGene(geneName: String, likePatterns: List<String>): RegexGene {
        val disjunctionRxGenes = likePatterns
                .map { createGeneForPostgresLike(it) }
                .map { it.disjunctions }
                .map { it.disjunctions }
                .flatten()
        return RegexGene(geneName, disjunctions = DisjunctionListRxGene(disjunctions = disjunctionRxGenes))
    }


    /**
     * Builds a RegexGene using a name and a list of SIMILAR_TO patterns.
     * The resulting gene is a disjunction of the given patterns
     * according to the database we are using
     */
    fun buildSimilarToRegexGene(
            geneName: String,
            similarToPatterns: List<String>,
            databaseType: DatabaseType
    ): RegexGene {
        return when {
            databaseType == DatabaseType.POSTGRES -> buildPostgresSimilarToRegexGene(geneName, similarToPatterns)
            //TODO: support other database SIMILAR_TO check expressions
            else -> throw UnsupportedOperationException(
                    "Must implement similarTo expressions for database %s".format(
                            databaseType
                    )
            )
        }
    }

    private fun buildPostgresSimilarToRegexGene(geneName: String, similarToPatterns: List<String>): RegexGene {
        val disjunctionRxGenes = similarToPatterns
                .map { createGeneForPostgresSimilarTo(it) }
                .map { it.disjunctions }
                .map { it.disjunctions }
                .flatten()
        return RegexGene(geneName, disjunctions = DisjunctionListRxGene(disjunctions = disjunctionRxGenes))
    }

    fun buildSqlTimestampGene(name: String): DateTimeGene {
        return DateTimeGene(
                name = name,
                date = DateGene(
                        "date",
                        year = IntegerGene("year", 2016, 1900, 2100),
                        month = IntegerGene("month", 3, 1, 12),
                        day = IntegerGene("day", 12, 1, 31),
                        onlyValidDates = true
                ),
                time = TimeGene(
                        "time",
                        hour = IntegerGene("hour", 0, 0, 23),
                        minute = IntegerGene("minute", 0, 0, 59),
                        second = IntegerGene("second", 0, 0, 59)
                ),
                dateTimeGeneFormat = DateTimeGene.DateTimeGeneFormat.DEFAULT_DATE_TIME
        )

    }

    private fun handleTimestampColumn(column: Column): DateTimeGene {
        if (column.enumValuesAsStrings != null) {
            throw RuntimeException("Unsupported enum in TIMESTAMP. Please implement")
        } else {
            return buildSqlTimestampGene(column.name)
        }
    }

    private fun handleCLOBColumn(column: Column): Gene {
        return if (column.enumValuesAsStrings != null) {
            checkNotEmpty(column.enumValuesAsStrings)
            EnumGene(name = column.name, data = column.enumValuesAsStrings)

        } else {
            StringGene(name = column.name, minLength = 0, maxLength = column.size)
        }
    }

    private fun handleBLOBColumn(column: Column): Gene {
        return if (column.enumValuesAsStrings != null) {
            checkNotEmpty(column.enumValuesAsStrings)
            EnumGene(name = column.name, data = column.enumValuesAsStrings)
        } else {
            StringGene(name = column.name, minLength = 0, maxLength = 8)
        }
    }

    private fun handleRealColumn(column: Column): Gene {
        /**
         * REAL is identical to the floating point statement float(24).
         * TODO How to discover if the source field is a float/Float field?
         */
        return if (column.enumValuesAsStrings != null) {
            checkNotEmpty(column.enumValuesAsStrings)
            EnumGene(name = column.name, data = column.enumValuesAsStrings.map { it.toDouble() })

        } else {
            DoubleGene(column.name)
        }
    }

    private fun handleDecimalColumn(column: Column): Gene {
        /**
         * TODO: DECIMAL precision is lower than a float gene
         */
        return if (column.enumValuesAsStrings != null) {
            checkNotEmpty(column.enumValuesAsStrings)
            EnumGene(name = column.name, data = column.enumValuesAsStrings.map { it.toFloat() })
        } else {
            if (column.scale != null && column.scale >= 0) {
                /*
                    set precision and boundary for DECIMAL
                    https://dev.mysql.com/doc/refman/8.0/en/fixed-point-types.html

                    for mysql, precision is [1, 65] (default 10), and scale is [0, 30] (default 0)
                    different db might have different range, then do not validate the range for the moment
                 */
                val range = NumberCalculationUtil.boundaryDecimal(column.size, column.scale)
                BigDecimalGene(
                        column.name,
                        min = if (column.isUnsigned) BigDecimal.ZERO.setScale(column.scale) else range.first,
                        max = range.second,
                        precision = column.size,
                        scale = column.scale
                )
            } else {
                if (column.scale == null) {
                    FloatGene(column.name)
                } else {
                    /*
                        TO check
                        with CompositeTypesTest for postgres,
                        the value of precision and scale is -1, may need to check with the authors
                     */
                    log.warn("invalid scale value (${column.scale}) for decimal, and it should not be less than 0")
                    if (column.size <= 0) {
                        log.warn("invalid precision value (${column.size}) for decimal, and it should not be less than 1")
                        FloatGene(column.name)
                    } else {
                        // for mysql, set the scale with default value 0 if it is invalid
                        BigDecimalGene(column.name, precision = column.size, scale = 0)
                    }
                }
            }
        }
    }

    private fun handleMoneyColumn(column: Column): Gene {
        return if (column.enumValuesAsStrings != null) {
            checkNotEmpty(column.enumValuesAsStrings)
            EnumGene(name = column.name, data = column.enumValuesAsStrings.map { it.toFloat() })
        } else {
            val MONEY_COLUMN_PRECISION = 2
            val MONEY_COLUMN_SIZE = 8
            val range = NumberCalculationUtil.boundaryDecimal(MONEY_COLUMN_SIZE, MONEY_COLUMN_PRECISION)

            BigDecimalGene(
                    column.name,
                    min = range.first,
                    max = range.second,
                    precision = MONEY_COLUMN_SIZE,
                    scale = MONEY_COLUMN_PRECISION
            )
        }
    }


    private fun handleBooleanColumn(column: Column): Gene {
        return if (column.enumValuesAsStrings != null) {
            checkNotEmpty(column.enumValuesAsStrings)
            EnumGene(column.name, column.enumValuesAsStrings.map { it.toBoolean() })

        } else {
            BooleanGene(column.name)
        }
    }

    private fun handleCompositeColumn(id: Long, table: Table, column: Column): Gene {
        if (column.compositeType == null) {
            throw IllegalArgumentException("Composite column should have a composite type for column ${column.name}")
        }
        val fields = column.compositeType
                .map { t -> buildGene(id, table, t) }
                .toList()
        return SqlCompositeGene(column.name, fields, column.compositeTypeName)
    }

    /**
     * Handle Postgres Object identifier type
     * (https://www.postgresql.org/docs/current/datatype-oid.html) as
     * integers.
     */
    private fun handleObjectIdentifierType(name: String) = IntegerGene(name)


    /**
     * handle bit for mysql
     * https://dev.mysql.com/doc/refman/8.0/en/bit-value-literals.html
     */
    private fun handleBitColumn(column: Column): Gene {
        val gene = SqlBitStringGene(column.name, minSize = column.size, maxSize = column.size)
        return gene
    }

    /**
     * handle bitvarying for postgres
     * https://www.postgresql.org/docs/14/datatype-bit.html
     */
    private fun handleBitVaryingColumn(column: Column): Gene {
        return SqlBitStringGene(column.name, minSize = 0, maxSize = column.size)
    }

    companion object {
        /**
         * Throws an exception if the enum values is empty
         * (parameter is non-nullable by definition)
         */
        private fun checkNotEmpty(enumValuesAsStrings: List<String>) {
            if (enumValuesAsStrings.isEmpty()) {
                throw IllegalArgumentException("the list of enumerated values cannot be empty")
            }
        }

        //  the real type has a range of around 1E-37 to 1E+37 with a precision of at least 6 decimal digits
        val MAX_FLOAT4_VALUE: Double = "1E38".toDouble()

        // The double precision type has a range of around 1E-307 to 1E+308 with a precision of at least 15 digits
        val MAX_FLOAT8_VALUE: Double = "1E308".toDouble()

        //  the real type has a range of around 1E-37 to 1E+37 with a precision of at least 6 decimal digits
        val MIN_FLOAT4_VALUE: Double = MAX_FLOAT4_VALUE.unaryMinus()

        // The double precision type has a range of around 1E-307 to 1E+308 with a precision of at least 15 digits
        val MIN_FLOAT8_VALUE: Double = MAX_FLOAT8_VALUE.unaryMinus()

        private val log: Logger = LoggerFactory.getLogger(DbActionGeneBuilder::class.java)
    }
}
