package org.evomaster.core.search.gene

import org.evomaster.core.search.service.AdaptiveParameterControl
import org.evomaster.core.search.service.Randomness
import org.evomaster.core.utils.NumberCalculationUtil.calculateIncrement
import java.math.RoundingMode

abstract class FloatingPointNumber<T:Number>(
    name: String,
    value: T?,
    min: T? = null,
    max: T? = null,
    minInclusive : Boolean,
    maxInclusive : Boolean,
    precision: Int?,
    scale: Int?
) : NumberGene<T>(name, value, min, max, minInclusive, maxInclusive, precision, scale){

    enum class ModifyStrategy{
        //for small changes
        SMALL_CHANGE,
        //for large jumps
        LARGE_JUMP,
        //to reduce precision, ie chop off digits after the "."
        REDUCE_PRECISION
    }


    private fun getMaxRange(direction: Double): Long {
        return if (!isRangeSpecified()) Long.MAX_VALUE
        else if (direction > 0)
            calculateIncrement(value.toDouble(), getMaximum().toDouble()).toLong()
        else
            calculateIncrement(getMinimum().toDouble(), value.toDouble()).toLong()
    }

    /**
     * mutate Floating Point Number in a standard way
     */
    fun mutateFloatingPointNumber(randomness: Randomness, apc: AdaptiveParameterControl): T{
        return NumberMutatorUtils.mutateFloatingPointNumber(randomness, null, maxRange = null, apc, value, smin = getMinimum(), smax = getMaximum(), scale=scale)
    }

    /**
     * @return formatted [value] based on [scale]
     */
    fun getFormattedValue(valueToFormat: T?=null, roundingMode: RoundingMode= RoundingMode.HALF_UP) : T{
        return NumberMutatorUtils.getFormattedValue(valueToFormat?:value, scale, roundingMode)
    }

    /**
     * @return minimal changes of the [value].
     * this is typically used when [scale] is specified
     */
    fun getMinimalDelta(): T{
        return NumberMutatorUtils.getDecimalEpsilon(scale, getZero())
    }

    /**
     * @return whether the gene is valid that considers
     *      1) within min..max if they are specified
     *      2) precision if it is specified
     */
    override fun isValid(): Boolean {
        return super.isValid() && (scale == null || !value.toString().contains(".") || value.toString().split(".")[1].length <= scale)
    }
}