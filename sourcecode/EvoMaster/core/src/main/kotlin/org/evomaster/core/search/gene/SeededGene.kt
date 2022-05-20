package org.evomaster.core.search.gene

import org.evomaster.core.output.OutputFormat
import org.evomaster.core.problem.util.ParamUtil
import org.evomaster.core.search.StructuralElement
import org.evomaster.core.search.impact.impactinfocollection.value.SeededGeneImpact
import org.evomaster.core.search.service.AdaptiveParameterControl
import org.evomaster.core.search.service.Randomness
import org.evomaster.core.search.service.mutator.MutationWeightControl
import org.evomaster.core.search.service.mutator.genemutation.AdditionalGeneMutationInfo
import org.evomaster.core.search.service.mutator.genemutation.SubsetGeneSelectionStrategy

/**
 * represent gene which contains seeded values customized by user with the driver
 */
class SeededGene<T : ComparableGene>(
    name: String,
    /**
     * the gene and its value could be randomized and handled during the search
     */
    val gene: T,
    /**
     * a set of candidates specified by user, the search could manipluate which one is applied
     */
    val seeded: EnumGene<T>,
    /**
     * representing if the [seeded] is applied to represent this
     * otherwise apply [gene]
     */
    var employSeeded: Boolean = false
) : Gene(name, mutableListOf(gene, seeded)) {

    /**
     * we might prevent search to manipulate the [employSeeded]
     */
    var isEmploySeededMutable = true
        private set


    /**
     * forbid changing [employSeeded] during the search
     */
    fun forbidEmploySeededMutable(){
        isEmploySeededMutable = false
    }

    override fun randomize(randomness: Randomness, forceNewValue: Boolean, allGenes: List<Gene>) {
        if (isEmploySeededMutable){
            if (forceNewValue) {
                employSeeded = !employSeeded
                return
            } else if (randomness.nextBoolean()){
                employSeeded = randomness.nextBoolean()
            }
        }

        if (!employSeeded)
            gene.randomize(randomness, forceNewValue, allGenes)
        else
            seeded.randomize(randomness, forceNewValue, allGenes)
    }

    override fun copyContent(): SeededGene<T> {
        val copy = SeededGene(name, gene.copyContent() as T, seeded.copyContent() as EnumGene<T>, employSeeded)
        copy.isEmploySeededMutable = this.isEmploySeededMutable
        return copy
    }

    /**
     * @return a gene representing [this]
     */
    fun getPhenotype() : T{
        return if (!employSeeded) gene else seeded.values[seeded.index]
    }

    override fun getValueAsPrintableString(
        previousGenes: List<Gene>,
        mode: GeneUtils.EscapeMode?,
        targetFormat: OutputFormat?,
        extraCheck: Boolean
    ): String {
        return if (employSeeded)
            seeded.getValueAsPrintableString(mode = mode, targetFormat = targetFormat)
        else
            gene.getValueAsPrintableString(mode = mode, targetFormat = targetFormat)
    }

    override fun copyValueFrom(other: Gene) {
        if (other !is SeededGene<*>)
            throw IllegalArgumentException("Invalid gene ${other::class.java}")
        this.employSeeded = other.employSeeded
        this.isEmploySeededMutable = other.isEmploySeededMutable
        if (employSeeded)
            this.seeded.copyValueFrom(other.seeded)
        else
            this.gene.copyValueFrom(other.gene)
    }

    override fun containsSameValueAs(other: Gene): Boolean {
        if (other !is SeededGene<*>)
            throw IllegalArgumentException("Invalid gene ${other::class.java}")
        return this.employSeeded == other.employSeeded
                && (if (employSeeded) this.seeded.containsSameValueAs(other.seeded) else this.gene.containsSameValueAs(other.gene))
    }

    override fun innerGene(): List<Gene> {
        return listOf(gene, seeded)
    }

    override fun possiblySame(gene : Gene) : Boolean = super.possiblySame(gene) && this.gene.possiblySame((gene as SeededGene<*>).gene)

    override fun bindValueBasedOn(gene: Gene): Boolean {
        // only allow bind value for gene
        if (gene is SeededGene<*> && isEmploySeededMutable){
            employSeeded = gene.employSeeded
            if (!employSeeded)
                return ParamUtil.getValueGene(this.gene).bindValueBasedOn(ParamUtil.getValueGene(gene.gene))
            else
                return seeded.bindValueBasedOn(gene.seeded)
        }

        if (gene !is SeededGene<*> && !employSeeded){
            return ParamUtil.getValueGene(this.gene).bindValueBasedOn(ParamUtil.getValueGene(gene))
        }

        return false
    }

    override fun getChildren(): List<out StructuralElement> {
        return listOf(gene, seeded)
    }

    override fun adaptiveSelectSubset(randomness: Randomness, internalGenes: List<Gene>, mwc: MutationWeightControl, additionalGeneMutationInfo: AdditionalGeneMutationInfo): List<Pair<Gene, AdditionalGeneMutationInfo?>> {
        if (additionalGeneMutationInfo.impact != null && additionalGeneMutationInfo.impact is SeededGeneImpact){
            if (internalGenes.size != 1)
                throw IllegalStateException("mismatched input: the internalGenes should only contain one candidate")
            return if (internalGenes.contains(gene))
                listOf(gene to additionalGeneMutationInfo.copyFoInnerGene(additionalGeneMutationInfo.impact.geneImpact, gene = gene))
            else if (internalGenes.contains(seeded))
                listOf(seeded to additionalGeneMutationInfo.copyFoInnerGene(additionalGeneMutationInfo.impact.seededGeneImpact, gene = seeded))
            else
                throw IllegalStateException("mismatched input: the internalGenes should contain either gene or seeded")
        }
        throw IllegalArgumentException("impact is null or not SeedGeneImpact")
    }

    override fun candidatesInternalGenes(
        randomness: Randomness,
        apc: AdaptiveParameterControl,
        allGenes: List<Gene>,
        selectionStrategy: SubsetGeneSelectionStrategy,
        enableAdaptiveGeneMutation: Boolean,
        additionalGeneMutationInfo: AdditionalGeneMutationInfo?
    ): List<Gene> {
        var changeEmploySeed = false
        if (isEmploySeededMutable){
            if (!enableAdaptiveGeneMutation || additionalGeneMutationInfo?.impact == null){
                changeEmploySeed = randomness.nextBoolean()
            }else{
                if (additionalGeneMutationInfo.impact is SeededGeneImpact){
                    changeEmploySeed = additionalGeneMutationInfo.impact.employSeedImpact.determinateSelect(
                        minManipulatedTimes = 5,
                        times = 1.5,
                        preferTrue = true,
                        targets = additionalGeneMutationInfo.targets,
                        selector = additionalGeneMutationInfo.archiveGeneSelector
                    )
                }else
                    throw IllegalArgumentException("impact is null or not SeededGeneImpact ${additionalGeneMutationInfo.impact}")
            }
        }

        if (changeEmploySeed)
            return emptyList()
        return listOf((if (employSeeded) seeded else gene))
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
        if (isEmploySeededMutable)
            employSeeded = !employSeeded
        return true
    }

    override fun isMutable(): Boolean {
        return isEmploySeededMutable || gene.isMutable() || seeded.isMutable()
    }

}