package org.evomaster.core.search.gene.sql.textsearch

import org.evomaster.core.search.gene.GeneUtils.SINGLE_APOSTROPHE_PLACEHOLDER
import org.evomaster.core.search.gene.StringGene
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test

class SqlTextSearchVectorGeneTest {

    @Test
    fun testEmptyTextSearchVector() {
        val gene = SqlTextSearchVectorGene("textSearchVector")
        (gene.innerGene()[0] as StringGene).value =""
        Assertions.assertEquals("to_tsvector(${SINGLE_APOSTROPHE_PLACEHOLDER}${SINGLE_APOSTROPHE_PLACEHOLDER})", gene.getValueAsPrintableString())
    }

    @Test
    fun testTextSearchVector() {
        val gene = SqlTextSearchVectorGene("textSearchVector")
        val textLexemes = gene.innerGene()[0] as StringGene
        textLexemes.value = "foo bar"
        Assertions.assertEquals("to_tsvector(${SINGLE_APOSTROPHE_PLACEHOLDER}foo bar${SINGLE_APOSTROPHE_PLACEHOLDER})", gene.getValueAsPrintableString())
    }


}