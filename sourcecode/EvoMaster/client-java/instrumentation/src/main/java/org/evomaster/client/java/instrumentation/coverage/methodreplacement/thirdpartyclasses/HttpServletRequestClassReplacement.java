package org.evomaster.client.java.instrumentation.coverage.methodreplacement.thirdpartyclasses;

import org.evomaster.client.java.instrumentation.coverage.methodreplacement.Replacement;
import org.evomaster.client.java.instrumentation.shared.ReplacementCategory;
import org.evomaster.client.java.instrumentation.coverage.methodreplacement.ThirdPartyMethodReplacementClass;
import org.evomaster.client.java.instrumentation.coverage.methodreplacement.UsageFilter;
import org.evomaster.client.java.instrumentation.shared.ReplacementType;
import org.evomaster.client.java.instrumentation.staticstate.ExecutionTracer;

import javax.servlet.ServletInputStream;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class HttpServletRequestClassReplacement extends ThirdPartyMethodReplacementClass {

    private static final HttpServletRequestClassReplacement singleton = new HttpServletRequestClassReplacement();



    @Override
    protected String getNameOfThirdPartyTargetClass() {
        return "javax.servlet.http.HttpServletRequest";
    }

    @Replacement(replacingStatic = false,
            type = ReplacementType.TRACKER,
            id = "getInputStream",
            usageFilter = UsageFilter.ONLY_SUT,
            category = ReplacementCategory.BASE)
    public static ServletInputStream getInputStream(Object caller) throws IOException {

        if(caller == null){
            throw new NullPointerException();
        }

        ExecutionTracer.markRawAccessOfHttpBodyPayload();

        Method original = getOriginal(singleton, "getInputStream", caller);

        try {
            return (ServletInputStream) original.invoke(caller);
        } catch (IllegalAccessException e){
            throw new RuntimeException(e);// ah, the beauty of Java...
        } catch (InvocationTargetException e){
            throw (RuntimeException) e.getCause();
        }
    }
}
