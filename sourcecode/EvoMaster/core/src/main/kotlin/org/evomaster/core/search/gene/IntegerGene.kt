package org.evomaster.core.search.gene

import org.evomaster.core.logging.LoggingUtil
import org.evomaster.core.output.OutputFormat
import org.evomaster.core.search.gene.GeneUtils.getDelta
import org.evomaster.core.search.gene.sql.SqlPrimaryKeyGene
import org.evomaster.core.search.service.AdaptiveParameterControl
import org.evomaster.core.search.service.Randomness
import org.evomaster.core.search.service.mutator.MutationWeightControl
import org.evomaster.core.search.service.mutator.genemutation.AdditionalGeneMutationInfo
import org.evomaster.core.search.service.mutator.genemutation.DifferentGeneInHistory
import org.evomaster.core.search.service.mutator.genemutation.SubsetGeneSelectionStrategy
import org.evomaster.core.utils.NumberCalculationUtil
import org.evomaster.core.utils.NumberCalculationUtil.upperBound
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.math.BigInteger
import kotlin.math.max
import kotlin.math.min


class IntegerGene(
    name: String,
    value: Int?,
    min: Int,
    max: Int,
    precision : Int?,
    minInclusive : Boolean,
    maxInclusive : Boolean,
) : IntegralNumberGene<Int>(name, value, min, max, precision, minInclusive, maxInclusive) {

    constructor(name: String, value: Int? = null, min: Int? = null, max: Int?=null, precision: Int?=null, minInclusive: Boolean = true, maxInclusive: Boolean = true) :this(
        name, value,
        min = (min?:Int.MIN_VALUE).run { if (precision!= null) max(this, (-upperBound(precision, 0)).toInt()) else this },
        max = (max?:Int.MAX_VALUE).run { if (precision!= null) min(this, (upperBound(precision, 0)).toInt()) else this },
        precision, minInclusive, maxInclusive)

    init {
        if (getMaximum() == getMinimum())
            this.value = getMinimum()
        if (getMaximum() < getMinimum())
            throw IllegalArgumentException("max must be greater than min but max is $max and min is $min")

    }

    companion object {
        private val log: Logger = LoggerFactory.getLogger(IntegerGene::class.java)
    }

    override fun copyContent(): Gene {
        return IntegerGene(name, value, precision = precision, min = min, max= max, minInclusive = minInclusive, maxInclusive = maxInclusive)
    }

    override fun copyValueFrom(other: Gene) {
        if (other !is IntegerGene) {
            throw IllegalArgumentException("Invalid gene type ${other.javaClass}")
        }
        this.value = other.value
    }

    override fun containsSameValueAs(other: Gene): Boolean {
        if (other !is IntegerGene) {
            throw IllegalArgumentException("Invalid gene type ${other.javaClass}")
        }
        return this.value == other.value
    }

    override fun randomize(randomness: Randomness, forceNewValue: Boolean, allGenes: List<Gene>) {

        value = randomness.randomizeBoundedIntAndLong(value.toLong(), getMinimum().toLong(), getMaximum().toLong(), forceNewValue).toInt()
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

        val mutated = super.mutate(randomness, apc, mwc, allGenes, selectionStrategy, enableAdaptiveGeneMutation, additionalGeneMutationInfo)
        if (mutated) return true

        //check maximum range. no point in having a delta greater than such range
        val range = getMaximum().toLong() - getMinimum().toLong()

        //choose an i for 2^i modification
        val delta = getDelta(randomness, apc, range)

        val sign = when (value) {
            max -> -1
            min -> +1
            else -> randomness.choose(listOf(-1, +1))
        }

        val res: Long = (value.toLong()) + (sign * delta)

        value = when {
            res > getMaximum() -> getMaximum()
            res < getMinimum() -> getMinimum()
            else -> res.toInt()
        }

        return true
    }


    override fun getValueAsPrintableString(
        previousGenes: List<Gene>,
        mode: GeneUtils.EscapeMode?,
        targetFormat: OutputFormat?,
        extraCheck: Boolean
    ): String {
        return value.toString()
    }

    override fun innerGene(): List<Gene> = listOf()

    override fun bindValueBasedOn(gene: Gene): Boolean {
        when (gene) {
            is IntegerGene -> value = gene.value
            is FloatGene -> value = gene.value.toInt()
            is DoubleGene -> value = gene.value.toInt()
            is LongGene -> value = gene.value.toInt()
            is BigDecimalGene -> value = try { gene.value.toInt() } catch (e: Exception) { return false }
            is BigIntegerGene -> value = try { gene.value.toInt() } catch (e: Exception) { return false }
            is StringGene -> {
                value = gene.value.toIntOrNull() ?: return false
            }
            is Base64StringGene -> {
                value = gene.data.value.toIntOrNull() ?: return false
            }
            is ImmutableDataHolderGene -> {
                value = gene.value.toIntOrNull() ?: return false
            }
            is SqlPrimaryKeyGene -> {
                value = gene.uniqueId.toInt()
            }
            is SeededGene<*> ->{
                return this.bindValueBasedOn(gene.getPhenotype())
            }
            is NumericStringGene ->{
                return this.bindValueBasedOn(gene.number)
            }
            else -> {
                LoggingUtil.uniqueWarn(log, "cannot bind Integer with ${gene::class.java.simpleName}")
                return false
            }
        }
        return true
    }

    override fun compareTo(other: ComparableGene): Int {
        if (other !is IntegerGene) {
            throw ClassCastException("Expected IntegerGene but " + other::javaClass + " was found.")
        }
        return this.toInt().compareTo(other.toInt())
    }

    override fun isMutable(): Boolean {
        return this.max!! > this.min!!
    }

    override fun getMaximum(): Int = max!!.run { if (!maxInclusive) this - 1 else this }

    override fun getMinimum(): Int = min!!.run { if (!minInclusive) this + 1 else this }


    override fun getDefaultValue(): Int {
        val df = super.getDefaultValue()
        if (df <= getMaximum() && df >= getMinimum())
            return df
        return NumberCalculationUtil.getMiddle(getMinimum(), getMaximum(), 0).toInt()
    }

    override fun getZero(): Int = 0
}