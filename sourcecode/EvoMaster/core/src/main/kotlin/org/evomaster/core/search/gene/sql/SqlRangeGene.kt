package org.evomaster.core.search.gene.sql

import org.evomaster.core.logging.LoggingUtil
import org.evomaster.core.output.OutputFormat
import org.evomaster.core.search.gene.*
import org.evomaster.core.search.service.AdaptiveParameterControl
import org.evomaster.core.search.service.Randomness
import org.evomaster.core.search.service.mutator.MutationWeightControl
import org.evomaster.core.search.service.mutator.genemutation.AdditionalGeneMutationInfo
import org.evomaster.core.search.service.mutator.genemutation.SubsetGeneSelectionStrategy
import org.slf4j.Logger
import org.slf4j.LoggerFactory


/**
 *  https://www.postgresql.org/docs/14/rangetypes.html
 *  A representation of numeric range type.
 */
class SqlRangeGene<T>(
        /**
         * The name of this gene
         */
        name: String,

        private val template: T,

        private val isLeftClosed: BooleanGene = BooleanGene("isLeftClosed"),

        private val left: T = template.copyContent() as T,

        private val right: T = template.copyContent() as T,

        private val isRightClosed: BooleanGene = BooleanGene("isRightClosed")

) : Gene(name, mutableListOf(isLeftClosed, left, right, isRightClosed))
        where T : ComparableGene {

    companion object {
        val log: Logger = LoggerFactory.getLogger(SqlRangeGene::class.java)
    }

    init {
        left.name = "left"
        right.name = "right"
        repairGeneIfNeeded()
    }


    private fun swapLeftRightValues() {
        val copyOfLeftGene = left.copy()
        left.copyValueFrom(right)
        right.copyValueFrom(copyOfLeftGene)
    }

    /**
     * LowerBound must always be less than or equal
     * to UpperBound
     */
    private fun repairGeneIfNeeded() {
        if (left > right) {
            swapLeftRightValues()
        }
        assert(left <= right)
    }

    override fun getChildren(): MutableList<Gene> =
            mutableListOf(isLeftClosed, left, right, isRightClosed)

    override fun copyContent(): Gene {
        return SqlRangeGene<T>(
                name = name,
                template = template.copyContent() as T,
                isLeftClosed = isLeftClosed.copyContent() as BooleanGene,
                left = left.copyContent() as T,
                right = right.copyContent() as T,
                isRightClosed = isRightClosed.copyContent() as BooleanGene
        )
    }

    override fun copyValueFrom(other: Gene) {
        if (other !is SqlRangeGene<*>) {
            throw IllegalArgumentException("Invalid gene type ${other.javaClass}")
        }

        isLeftClosed.copyValueFrom(other.isLeftClosed)
        left.copyValueFrom(other.left)
        right.copyValueFrom(other.right)
        isRightClosed.copyValueFrom(other.isRightClosed)
    }

    override fun containsSameValueAs(other: Gene): Boolean {
        if (other !is SqlRangeGene<*>) {
            throw IllegalArgumentException("Invalid gene type ${other.javaClass}")
        }
        return isLeftClosed.containsSameValueAs(other.isRightClosed)
                && left.containsSameValueAs(other.left)
                && right.containsSameValueAs(other.right)
                && isRightClosed.containsSameValueAs(other.isRightClosed)
    }


    override fun randomize(randomness: Randomness, forceNewValue: Boolean, allGenes: List<Gene>) {
        log.trace("Randomizing SqlRangeGene")
        listOf(isRightClosed, left, right, isRightClosed)
                .forEach { it.randomize(randomness, forceNewValue, allGenes) }
        repairGeneIfNeeded()
    }

    /**
     * Forbid explicitly individual mutation
     * of these genes
     */
    override fun candidatesInternalGenes(
            randomness: Randomness,
            apc: AdaptiveParameterControl,
            allGenes: List<Gene>,
            selectionStrategy: SubsetGeneSelectionStrategy,
            enableAdaptiveGeneMutation: Boolean,
            additionalGeneMutationInfo: AdditionalGeneMutationInfo?
    ): List<Gene> {
        return listOf()
    }

    private fun isLeftOpen(): Boolean {
        return !isLeftClosed.value
    }

    private fun isRightOpen(): Boolean {
        return !isRightClosed.value
    }

    private fun isEmpty(): Boolean {
        return (isLeftOpen() || isRightOpen()) &&
                left.containsSameValueAs(right)
    }

    override fun getValueAsPrintableString(
            previousGenes: List<Gene>,
            mode: GeneUtils.EscapeMode?,
            targetFormat: OutputFormat?,
            extraCheck: Boolean
    ): String {
        if (isEmpty())
            return "\"empty\""
        else
            return String.format(
                    "\"%s %s , %s %s\"",
                    if (isRightOpen()) '(' else '[',
                    left.getValueAsRawString(),
                    right.getValueAsRawString(),
                    if (isLeftOpen()) ')' else ']'
            )
    }


    override fun flatView(excludePredicate: (Gene) -> Boolean): List<Gene> {
        return if (excludePredicate(this)) listOf(this) else
            listOf(this).plus(isLeftClosed)
                    .plus(left)
                    .plus(right)
                    .plus(isRightClosed)
    }


    override fun innerGene(): List<Gene> =
            listOf(isLeftClosed, left, right, isRightClosed)

    override fun bindValueBasedOn(gene: Gene): Boolean {
        if (gene is SqlRangeGene<*> && gene.template::class.java.simpleName == template::class.java.simpleName) {
            this.isLeftClosed.bindValueBasedOn(gene.isLeftClosed)
            this.left.bindValueBasedOn(gene.left)
            this.right.bindValueBasedOn(gene.right)
            this.isRightClosed.bindValueBasedOn(gene.isRightClosed)
        }
        LoggingUtil.uniqueWarn(
                log,
                "cannot bind SqlNumericRangeGene with the template (${template::class.java.simpleName}) with ${gene::class.java.simpleName}"
        )
        return false
    }


    override fun mutate(
            randomness: Randomness,
            apc: AdaptiveParameterControl,
            mwc: MutationWeightControl,
            allGenes: List<Gene>,
            selectionStrategy: SubsetGeneSelectionStrategy,
            enableAdaptiveGeneMutation: Boolean,
            additionalGeneMutationInfo: AdditionalGeneMutationInfo?
    ): Boolean {
        this.randomize(randomness, true, allGenes)
        return true
    }

}