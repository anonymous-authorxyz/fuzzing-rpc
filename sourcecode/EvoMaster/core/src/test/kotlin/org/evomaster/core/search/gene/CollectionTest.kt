package org.evomaster.core.search.gene

import org.evomaster.core.search.service.Randomness
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.CsvSource

internal class CollectionTest {


    @ParameterizedTest
    @CsvSource(
        "0, 2147483647, ${MapGene.MAX_SIZE}, ${MapGene.MAX_SIZE}",
        "5, 2147483647, ${5+MapGene.MAX_SIZE}, ${5+MapGene.MAX_SIZE}",
        "0, 2, 2, ${MapGene.MAX_SIZE}"
    )
    fun testMapWithHugeMaxSizeInRandomize(min: Int, max: Int, expected: Int, defaultMax: Int){
        val pairGene = PairGene("template",IntegerGene("key", 1), LongGene("value", 1))
        val mapGene = MapGene("test", pairGene, maxSize = max, minSize = min)
        mapGene.randomize(Randomness(), false, listOf())
        assertEquals(expected, mapGene.getMaxSizeUsedInRandomize())
        assertEquals(defaultMax, mapGene.getDefaultMaxSize())
        assertTrue(mapGene.getAllElements().size <= mapGene.getMaxSizeUsedInRandomize())
        assertTrue(mapGene.getAllElements().size >= mapGene.getMinSizeOrDefault())
    }


    @ParameterizedTest
    @CsvSource(
        "0, 2147483647, ${MapGene.MAX_SIZE}, ${MapGene.MAX_SIZE}",
        "5, 2147483647, ${5+MapGene.MAX_SIZE}, ${5+MapGene.MAX_SIZE}",
        "0, 2, 2, ${MapGene.MAX_SIZE}"
    )
    fun testArrayWithHugeMaxSizeInRandomize(min: Int, max: Int, expected: Int, defaultMax: Int){
        val arrayGene = ArrayGene("test", LongGene("element"), maxSize = max, minSize = min)
        arrayGene.randomize(Randomness(), false, listOf())
        assertEquals(expected, arrayGene.getMaxSizeUsedInRandomize())
        assertEquals(defaultMax, arrayGene.getDefaultMaxSize())
        assertTrue(arrayGene.getAllElements().size <= arrayGene.getMaxSizeUsedInRandomize())
        assertTrue(arrayGene.getAllElements().size >= arrayGene.getMinSizeOrDefault())

    }

}