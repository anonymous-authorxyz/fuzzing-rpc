package org.evomaster.core.search.gene

import org.evomaster.core.output.OutputFormat
import org.evomaster.core.search.service.AdaptiveParameterControl
import org.evomaster.core.search.service.Randomness
import org.evomaster.core.search.service.mutator.genemutation.AdditionalGeneMutationInfo
import org.evomaster.core.search.service.mutator.genemutation.SubsetGeneSelectionStrategy

/**
 * handling pair type or Map.Entry type
 */
class PairGene<F,S>(
        name: String,
        val first: F,
        val second : S,
        /**
         *
         * whether the [first] is mutable
         */
        val isFirstMutable : Boolean = true
):Gene(name, listOf(first, second)) where F: Gene, S: Gene{

    companion object{

        /**
         * create simple pair gene based on [gene]
         * first is a StringGene and its name is based on the name of [gene]
         * second is [gene]
         * @param gene is the second of the pair
         * @param isFixedFirst specifies whether the first is fixed value.
         */
        fun <T:Gene> createStringPairGene(gene: T, isFixedFirst: Boolean = false) : PairGene<StringGene, T>{
            val key = StringGene(gene.name)
            if (isFixedFirst)
                key.value = gene.name
            return PairGene(gene.name, key, gene, isFirstMutable = !isFixedFirst)
        }

    }



    override fun randomize(randomness: Randomness, forceNewValue: Boolean, allGenes: List<Gene>) {
        first.randomize(randomness, forceNewValue, allGenes)
        second.randomize(randomness, forceNewValue, allGenes)
    }

    override fun getValueAsPrintableString(previousGenes: List<Gene>, mode: GeneUtils.EscapeMode?, targetFormat: OutputFormat?, extraCheck: Boolean): String {
        return "${first.getValueAsPrintableString(targetFormat = targetFormat)}:${second.getValueAsPrintableString(targetFormat = targetFormat)}"
    }

    override fun flatView(excludePredicate: (Gene) -> Boolean): List<Gene>{
        return if (excludePredicate(this)) listOf(this)
        else listOf(this).plus(listOf(first,second).flatMap { g -> g.flatView(excludePredicate) })
    }

    override fun copyValueFrom(other: Gene) {
        if (other !is PairGene<*,*>) {
            throw IllegalArgumentException("Invalid gene type ${other.javaClass}")
        }
        first.copyValueFrom(other.first)
        second.copyValueFrom(other.second)
    }

    override fun containsSameValueAs(other: Gene): Boolean {
        if (other !is PairGene<*,*>) {
            throw IllegalArgumentException("Invalid gene type ${other.javaClass}")
        }
        return first.containsSameValueAs(other.first) && second.containsSameValueAs(other.second)
    }

    override fun innerGene(): List<Gene> {
        return listOf(first, second)
    }

    override fun bindValueBasedOn(gene: Gene): Boolean {
        if (gene !is PairGene<*,*>) {
            throw IllegalArgumentException("Invalid gene type ${gene.javaClass}")
        }
        return first.bindValueBasedOn(gene.first) && second.bindValueBasedOn(gene.second)
    }

    override fun getChildren(): List<Gene> {
        return listOf(first, second)
    }


    override fun copyContent(): Gene {
        return PairGene(name, first.copyContent(), second.copyContent(), isFirstMutable)
    }

    override fun isMutable(): Boolean {
        return (first.isMutable() && isFirstMutable) || second.isMutable()
    }

    override fun isPrintable(): Boolean {
        return first.isPrintable() && second.isPrintable()
    }

    override fun candidatesInternalGenes(randomness: Randomness, apc: AdaptiveParameterControl, allGenes: List<Gene>, selectionStrategy: SubsetGeneSelectionStrategy, enableAdaptiveGeneMutation: Boolean, additionalGeneMutationInfo: AdditionalGeneMutationInfo?): List<Gene> {
        val list = mutableListOf<Gene>()
        if (first.isMutable() && isFirstMutable)
            list.add(first)
        if (second.isMutable())
            list.add(second)
        return list
    }

    override fun mutationWeight(): Double {
        return (if (isFirstMutable) first.mutationWeight() else 0.0) + second.mutationWeight()
    }
}