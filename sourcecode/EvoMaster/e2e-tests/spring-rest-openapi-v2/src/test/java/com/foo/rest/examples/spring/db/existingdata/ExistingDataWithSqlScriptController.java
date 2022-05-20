package com.foo.rest.examples.spring.db.existingdata;

import com.foo.rest.examples.spring.db.SpringWithDbController;
import org.evomaster.client.java.controller.internal.db.DbSpecification;
import org.springframework.util.ResourceUtils;

import java.io.FileNotFoundException;
import java.util.List;

public class ExistingDataWithSqlScriptController extends SpringWithDbController {

    public ExistingDataWithSqlScriptController() {
        super(ExistingDataApp.class);
    }

    @Override
    public void resetStateOfSUT() {
        super.resetStateOfSUT();
    }

    @Override
    public List<DbSpecification> getDbSpecifications() {
        List<DbSpecification> spec =  super.getDbSpecifications();
        if (spec != null && !spec.isEmpty()) {
            spec.get(0).initSqlOnResourcePath = "/sql/existingdata.sql";
        }
        return spec;
    }
}
