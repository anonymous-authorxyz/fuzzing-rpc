package org.evomaster.core.search.gene.sql.geometric

import org.evomaster.core.search.gene.*
import org.evomaster.core.logging.LoggingUtil
import org.evomaster.core.search.service.Randomness
import org.slf4j.Logger
import org.slf4j.LoggerFactory

class SqlLineGene(
    name: String,
    p: SqlPointGene = SqlPointGene(
        "p",
        x = FloatGene("x", value = 0.0f),
        y = FloatGene("y", value = 0.0f)
    ),
    q: SqlPointGene = SqlPointGene(
        "q",
        x = FloatGene("x", value = 1.0f),
        y = FloatGene("y", value = 1.0f)
    )
) : AbstractGeometricGene(name, p, q) {

    companion object {
        val log: Logger = LoggerFactory.getLogger(SqlLineGene::class.java)
    }

    override fun copyContent(): Gene = SqlLineGene(
        name,
        p.copyContent() as SqlPointGene,
        q.copyContent() as SqlPointGene
    )

    override fun copyValueFrom(other: Gene) {
        if (other !is SqlLineGene) {
            throw IllegalArgumentException("Invalid gene type ${other.javaClass}")
        }
        this.p.copyValueFrom(other.p)
        this.q.copyValueFrom(other.q)
    }

    override fun containsSameValueAs(other: Gene): Boolean {
        if (other !is SqlLineGene) {
            throw IllegalArgumentException("Invalid gene type ${other.javaClass}")
        }
        return this.p.containsSameValueAs(other.p)
                && this.q.containsSameValueAs(other.q)
    }

    override fun bindValueBasedOn(gene: Gene): Boolean {
        return when {
            gene is SqlLineGene -> {
                p.bindValueBasedOn(gene.p) &&
                        q.bindValueBasedOn(gene.q)
            }
            else -> {
                LoggingUtil.uniqueWarn(log, "cannot bind PointGene with ${gene::class.java.simpleName}")
                false
            }
        }
    }

    override fun randomize(randomness: Randomness, forceNewValue: Boolean, allGenes: List<Gene>) {
        super.randomize(randomness, forceNewValue, allGenes)
        /*
         * Lines cannot contain the same p,q points
         */
        if (p.x.equals(q.x) && p.y.equals(q.y)) {
            p.x.value = p.x.value + 1
        }
    }
}