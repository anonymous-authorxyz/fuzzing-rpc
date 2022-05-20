package org.evomaster.core.search.gene.sql.network

import org.evomaster.core.logging.LoggingUtil
import org.evomaster.core.output.OutputFormat
import org.evomaster.core.search.gene.Gene
import org.evomaster.core.search.gene.GeneUtils
import org.evomaster.core.search.gene.IntegerGene
import org.evomaster.core.search.service.AdaptiveParameterControl
import org.evomaster.core.search.service.Randomness
import org.evomaster.core.search.service.mutator.genemutation.AdditionalGeneMutationInfo
import org.evomaster.core.search.service.mutator.genemutation.SubsetGeneSelectionStrategy
import org.slf4j.Logger
import org.slf4j.LoggerFactory

/**
 * https://www.postgresql.org/docs/14/datatype-net-types.html#DATATYPE-MACADDR
 * Gene type for 6 and 8 byte MAC addresses.
 */
class SqlMacAddrGene(
        name: String,
        numberOfOctets: Int = MACADDR6_SIZE,
        private val octets: List<IntegerGene> = List(numberOfOctets)
        { i -> IntegerGene("b$i", min = 0, max = 255) }
) : Gene(name, octets.toMutableList()) {

    companion object {

        const val MACADDR6_SIZE = 6

        const val MACADDR8_SIZE = 8

        val log: Logger = LoggerFactory.getLogger(SqlMacAddrGene::class.java)
    }

    override fun getChildren(): MutableList<Gene> = octets.toMutableList()

    override fun randomize(randomness: Randomness, forceNewValue: Boolean, allGenes: List<Gene>) {
        octets.forEach { it.randomize(randomness, forceNewValue, allGenes) }
    }

    override fun candidatesInternalGenes(
            randomness: Randomness,
            apc: AdaptiveParameterControl,
            allGenes: List<Gene>,
            selectionStrategy: SubsetGeneSelectionStrategy,
            enableAdaptiveGeneMutation: Boolean,
            additionalGeneMutationInfo: AdditionalGeneMutationInfo?
    ): List<Gene> {
        return octets.toList()
    }

    override fun getValueAsPrintableString(
            previousGenes: List<Gene>,
            mode: GeneUtils.EscapeMode?,
            targetFormat: OutputFormat?,
            extraCheck: Boolean
    ): String {
        return "\"${
            octets
                    .map { String.format("%02X", it.value) }
                    .joinToString(":")
        }\""
    }

    override fun getValueAsRawString(): String {
        return octets
                .map { Integer.toHexString(it.value) }
                .joinToString(":")

    }


    override fun flatView(excludePredicate: (Gene) -> Boolean): List<Gene> {
        if (excludePredicate(this))
            return listOf(this)
        else {
            val result = listOf(this)
            octets.forEach { result.plus(it.flatView(excludePredicate)) }
            return result
        }
    }

    override fun innerGene(): List<Gene> = octets.toList()

    override fun bindValueBasedOn(gene: Gene): Boolean {
        return when {
            gene is SqlMacAddrGene -> {
                var result = true
                repeat(octets.size) {
                    result = result && octets[it].bindValueBasedOn(gene.octets[it])
                }
                result
            }
            else -> {
                LoggingUtil.uniqueWarn(log, "cannot bind MacAddrGene with ${gene::class.java.simpleName}")
                false
            }
        }
    }

    override fun copyValueFrom(other: Gene) {
        if (other !is SqlMacAddrGene) {
            throw IllegalArgumentException("Invalid gene type ${other.javaClass}")
        }
        if (octets.size != other.octets.size) {
            throw IllegalArgumentException(
                    "cannot bind MacAddrGene${octets.size} with MacAddrGene${other.octets.size}"
            )
        }
        repeat(octets.size) {
            octets[it].copyValueFrom(other.octets[it])
        }
    }

    override fun containsSameValueAs(other: Gene): Boolean {
        if (other !is SqlMacAddrGene) {
            throw IllegalArgumentException("Invalid gene type ${other.javaClass}")
        }
        if (octets.size != other.octets.size) {
            return false
        }
        var result = true
        repeat(octets.size) {
            result = result && octets[it].containsSameValueAs(other.octets[it])
        }
        return result
    }

    fun size() = octets.size

    override fun copyContent() = SqlMacAddrGene(name, numberOfOctets = octets.size, octets.map { it.copyContent() as IntegerGene }.toList())
}