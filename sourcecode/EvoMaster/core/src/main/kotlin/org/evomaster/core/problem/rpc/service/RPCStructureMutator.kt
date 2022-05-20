package org.evomaster.core.problem.rpc.service

import com.google.inject.Inject
import org.evomaster.core.database.SqlInsertBuilder
import org.evomaster.core.problem.httpws.service.ApiWsStructureMutator
import org.evomaster.core.problem.rpc.RPCIndividual
import org.evomaster.core.search.EvaluatedIndividual
import org.evomaster.core.search.Individual
import org.evomaster.core.search.service.mutator.MutatedGeneSpecification

/**
 * created by manzhang on 2021/11/26
 */
class RPCStructureMutator : ApiWsStructureMutator() {

    @Inject
    private lateinit var sampler: RPCSampler

    override fun mutateStructure(individual: Individual, evaluatedIndividual: EvaluatedIndividual<*>, mutatedGenes: MutatedGeneSpecification?, targets: Set<Int>) {
        if (individual !is RPCIndividual) {
            throw IllegalArgumentException("Invalid: individual type to be mutated with RPCStructureMutator should be RPCIndividual but ${individual::class.java.simpleName}")
        }

        if (!individual.canMutateStructure()) return
        if (config.maxTestSize == 1) return

        mutateForRandomType(individual, mutatedGenes)
        /*
            TODO Man other smart strategies
         */
        if (config.trackingEnabled()) tag(individual, time.evaluatedIndividuals)
    }

    private fun mutateForRandomType(individual: RPCIndividual, mutatedGenes: MutatedGeneSpecification?) {
        val size = individual.seeActions().size
        if ((size + 1 < config.maxTestSize) && (size == 1 || randomness.nextBoolean())){
            // add
            val sampledAction = sampler.sampleRandomAction()

            //save mutated genes
            mutatedGenes?.addRemovedOrAddedByAction(sampledAction, size, false, size)
            individual.addAction(action = sampledAction)
        }else{
            // remove
            val chosen = randomness.nextInt(size)
            val removed = individual.seeActions()[chosen]
            //save mutated genes
            mutatedGenes?.addRemovedOrAddedByAction(removed, size, true, size)
            individual.removeAction(chosen)
        }
    }

    override fun addInitializingActions(individual: EvaluatedIndividual<*>, mutatedGenes: MutatedGeneSpecification?) {
        addInitializingActions(individual, mutatedGenes, sampler)
    }

    override fun getSqlInsertBuilder(): SqlInsertBuilder? {
        return sampler.sqlInsertBuilder
    }
}