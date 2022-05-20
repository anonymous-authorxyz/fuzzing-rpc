package org.evomaster.core.search.gene

import org.evomaster.core.output.OutputFormat
import org.evomaster.core.search.impact.impactinfocollection.value.OptionalGeneImpact
import org.evomaster.core.search.impact.impactinfocollection.value.numeric.NumericStringGeneImpact
import org.evomaster.core.search.service.AdaptiveParameterControl
import org.evomaster.core.search.service.Randomness
import org.evomaster.core.search.service.mutator.MutationWeightControl
import org.evomaster.core.search.service.mutator.genemutation.AdditionalGeneMutationInfo
import org.evomaster.core.search.service.mutator.genemutation.SubsetGeneSelectionStrategy
import java.math.BigDecimal

/**
 * A string to represent a number in all aspects,
 * but in its phenotype it is going to be outputted as a string
 */
class NumericStringGene(
    name: String,
    /**
     * inclusive
     *
     * note that precision would represent maxLength
     *
     * TODO
     * update min and max if the minLength is not 0
     */
    val minLength: Int,
    val number : BigDecimalGene
) : ComparableGene(name, mutableListOf(number)) {

    constructor(name: String,
                minLength: Int = 0,
                value: BigDecimal? = null,
                min : BigDecimal? = null,
                max : BigDecimal? = null,
                minInclusive : Boolean = true,
                maxInclusive : Boolean = true,
                floatingPointMode : Boolean = true,
                precision : Int? = null,
                scale : Int? = null) : this(name, minLength, BigDecimalGene(name, value, min, max, minInclusive, maxInclusive, floatingPointMode, precision?:if (scale == 0) 20 else 15, scale))


    override fun getChildren(): List<Gene> = mutableListOf(number)

    override fun copyContent(): Gene {
        return NumericStringGene(name, minLength, number.copyContent())
    }

    override fun isMutable(): Boolean {
        return number.isMutable()
    }

    override fun copyValueFrom(other: Gene) {
        if (other !is NumericStringGene)
            throw IllegalArgumentException("Invalid gene type ${other.javaClass}")
        this.number.copyValueFrom(other.number)
    }

    override fun containsSameValueAs(other: Gene): Boolean {
        if (other !is NumericStringGene)
            throw IllegalArgumentException("Invalid gene type ${other.javaClass}")
        return this.number.containsSameValueAs(other.number)
    }

    override fun randomize(randomness: Randomness, forceNewValue: Boolean, allGenes: List<Gene>) {
        number.randomize(randomness, forceNewValue, allGenes)
    }

    override fun candidatesInternalGenes(
        randomness: Randomness,
        apc: AdaptiveParameterControl,
        allGenes: List<Gene>,
        selectionStrategy: SubsetGeneSelectionStrategy,
        enableAdaptiveGeneMutation: Boolean,
        additionalGeneMutationInfo: AdditionalGeneMutationInfo?
    ): List<Gene> {
        return listOf(number)
    }

    override fun adaptiveSelectSubset(
        randomness: Randomness,
        internalGenes: List<Gene>,
        mwc: MutationWeightControl,
        additionalGeneMutationInfo: AdditionalGeneMutationInfo
    ): List<Pair<Gene, AdditionalGeneMutationInfo?>> {
        if (additionalGeneMutationInfo.impact != null && additionalGeneMutationInfo.impact is NumericStringGeneImpact){
            return listOf(number to additionalGeneMutationInfo.copyFoInnerGene(additionalGeneMutationInfo.impact.numberGeneImpact, gene = number))
        }
        throw IllegalArgumentException("impact is null or not OptionalGeneImpact")
    }

    override fun getValueAsPrintableString(
        previousGenes: List<Gene>,
        mode: GeneUtils.EscapeMode?,
        targetFormat: OutputFormat?,
        extraCheck: Boolean
    ): String {
        // avoid scientific representation if the number is string
        return "\"" + number.value.toPlainString() + "\""
    }

    override fun flatView(excludePredicate: (Gene) -> Boolean): List<Gene> {
        return if(excludePredicate(this)) listOf(this) else listOf(this).plus(number.flatView(excludePredicate))
    }

    override fun innerGene(): List<Gene> = listOf()

    override fun bindValueBasedOn(gene: Gene): Boolean {
        return when(gene){
            is NumericStringGene -> number.bindValueBasedOn(gene.number)
            else-> number.bindValueBasedOn(gene)
        }
    }

    override fun compareTo(other: ComparableGene): Int {
        if (other !is NumericStringGene) {
            throw IllegalArgumentException("Invalid gene type ${other.javaClass}")
        }
        return number.compareTo(other.number)
    }
}