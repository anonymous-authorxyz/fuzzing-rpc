package org.evomaster.core.search.gene.sql.geometric

import org.evomaster.core.search.gene.*
import org.evomaster.core.logging.LoggingUtil
import org.evomaster.core.output.OutputFormat
import org.evomaster.core.search.service.AdaptiveParameterControl
import org.evomaster.core.search.service.Randomness
import org.evomaster.core.search.service.mutator.genemutation.AdditionalGeneMutationInfo
import org.evomaster.core.search.service.mutator.genemutation.SubsetGeneSelectionStrategy
import org.slf4j.Logger
import org.slf4j.LoggerFactory

class SqlCircleGene(
        name: String,
        val c: SqlPointGene = SqlPointGene(name = "c"),
        // radius cannot be negative
        val r: FloatGene = FloatGene(name = "r", min = 0f, minInclusive = true)
) : Gene(name, mutableListOf(c, r)) {

    companion object {
        val log: Logger = LoggerFactory.getLogger(SqlCircleGene::class.java)
    }

    override fun getChildren(): MutableList<Gene> = mutableListOf(c, r)

    override fun copyContent(): Gene = SqlCircleGene(
            name,
            c.copyContent() as SqlPointGene,
            r.copyContent()
    )

    override fun randomize(randomness: Randomness, forceNewValue: Boolean, allGenes: List<Gene>) {
        c.randomize(randomness, forceNewValue, allGenes)
        r.randomize(randomness, forceNewValue, allGenes)
    }

    override fun candidatesInternalGenes(
            randomness: Randomness,
            apc: AdaptiveParameterControl,
            allGenes: List<Gene>,
            selectionStrategy: SubsetGeneSelectionStrategy,
            enableAdaptiveGeneMutation: Boolean,
            additionalGeneMutationInfo: AdditionalGeneMutationInfo?
    ): List<Gene> {
        return listOf(c, r)
    }

    override fun getValueAsPrintableString(
            previousGenes: List<Gene>,
            mode: GeneUtils.EscapeMode?,
            targetFormat: OutputFormat?,
            extraCheck: Boolean
    ): String {
        return "\" ( ${c.getValueAsRawString()} , ${r.getValueAsRawString()} ) \""
    }

    override fun getValueAsRawString(): String {
        return "(${c.getValueAsRawString()} , ${r.getValueAsRawString()})"
    }

    override fun copyValueFrom(other: Gene) {
        if (other !is SqlCircleGene) {
            throw IllegalArgumentException("Invalid gene type ${other.javaClass}")
        }
        this.c.copyValueFrom(other.c)
        this.r.copyValueFrom(other.r)
    }

    override fun containsSameValueAs(other: Gene): Boolean {
        if (other !is SqlCircleGene) {
            throw IllegalArgumentException("Invalid gene type ${other.javaClass}")
        }
        return this.c.containsSameValueAs(other.c)
                && this.r.containsSameValueAs(other.r)
    }

    override fun flatView(excludePredicate: (Gene) -> Boolean): List<Gene> {
        return if (excludePredicate(this)) listOf(this) else
            listOf(this).plus(c.flatView(excludePredicate)).plus(r.flatView(excludePredicate))
    }

    override fun innerGene(): List<Gene> = listOf(c, r)

    override fun bindValueBasedOn(gene: Gene): Boolean {
        return when {
            gene is SqlCircleGene -> {
                c.bindValueBasedOn(gene.c) &&
                        r.bindValueBasedOn(gene.r)
            }
            else -> {
                LoggingUtil.uniqueWarn(log, "cannot bind CircleGene with ${gene::class.java.simpleName}")
                false
            }
        }
    }


}