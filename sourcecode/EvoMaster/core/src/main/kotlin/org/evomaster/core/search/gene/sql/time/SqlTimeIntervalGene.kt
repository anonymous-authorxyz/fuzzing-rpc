package org.evomaster.core.search.gene.sql.time

import org.evomaster.core.logging.LoggingUtil
import org.evomaster.core.output.OutputFormat
import org.evomaster.core.search.gene.Gene
import org.evomaster.core.search.gene.GeneUtils
import org.evomaster.core.search.gene.IntegerGene
import org.evomaster.core.search.gene.datetime.TimeGene
import org.evomaster.core.search.service.AdaptiveParameterControl
import org.evomaster.core.search.service.Randomness
import org.evomaster.core.search.service.mutator.genemutation.AdditionalGeneMutationInfo
import org.evomaster.core.search.service.mutator.genemutation.SubsetGeneSelectionStrategy
import org.slf4j.Logger
import org.slf4j.LoggerFactory

class SqlTimeIntervalGene(
        name: String,
        val days: IntegerGene = IntegerGene(name = "days", min = 0),
        val time: TimeGene = TimeGene(
                "hoursMinutesAndSeconds",
                timeGeneFormat = TimeGene.TimeGeneFormat.ISO_LOCAL_DATE_FORMAT
        )
) : Gene(name, mutableListOf(days, time)) {

    companion object {
        val log: Logger = LoggerFactory.getLogger(SqlTimeIntervalGene::class.java)
    }

    override fun getChildren(): MutableList<Gene> = mutableListOf(days, time)

    override fun copyContent(): Gene = SqlTimeIntervalGene(
            name,
            days.copyContent() as IntegerGene,
            time.copyContent() as TimeGene
    )

    override fun randomize(randomness: Randomness, forceNewValue: Boolean, allGenes: List<Gene>) {
        /**
         * If forceNewValue==true both date and time
         * get a new value, but it only might need
         * one to be different to get a new value.
         *
         * Shouldn't this method decide randomly if
         * date, time or both get a new value?
         */
        days.randomize(randomness, forceNewValue, allGenes)
        time.randomize(randomness, forceNewValue, allGenes)
    }

    override fun candidatesInternalGenes(
            randomness: Randomness,
            apc: AdaptiveParameterControl,
            allGenes: List<Gene>,
            selectionStrategy: SubsetGeneSelectionStrategy,
            enableAdaptiveGeneMutation: Boolean,
            additionalGeneMutationInfo: AdditionalGeneMutationInfo?
    ): List<Gene> {
        return listOf(days, time)
    }

    override fun getValueAsPrintableString(
            previousGenes: List<Gene>,
            mode: GeneUtils.EscapeMode?,
            targetFormat: OutputFormat?,
            extraCheck: Boolean
    ): String {
        return "\"${days.value} days ${time.getValueAsRawString()}\""
    }

    override fun getValueAsRawString(): String {
        return "${days.value} days ${time.getValueAsRawString()}"
    }

    override fun copyValueFrom(other: Gene) {
        if (other !is SqlTimeIntervalGene) {
            throw IllegalArgumentException("Invalid gene type ${other.javaClass}")
        }
        this.days.copyValueFrom(other.days)
        this.time.copyValueFrom(other.time)
    }

    override fun containsSameValueAs(other: Gene): Boolean {
        if (other !is SqlTimeIntervalGene) {
            throw IllegalArgumentException("Invalid gene type ${other.javaClass}")
        }
        return this.days.containsSameValueAs(other.days)
                && this.time.containsSameValueAs(other.time)
    }

    override fun flatView(excludePredicate: (Gene) -> Boolean): List<Gene> {
        return if (excludePredicate(this)) listOf(this) else
            listOf(this).plus(days.flatView(excludePredicate)).plus(time.flatView(excludePredicate))
    }

    override fun innerGene(): List<Gene> = listOf(days, time)

    override fun bindValueBasedOn(gene: Gene): Boolean {
        return when {
            gene is SqlTimeIntervalGene -> {
                days.bindValueBasedOn(gene.days) &&
                        time.bindValueBasedOn(gene.time)
            }
            else -> {
                LoggingUtil.uniqueWarn(log, "cannot bind IntervalGene with ${gene::class.java.simpleName}")
                false
            }
        }
    }



}