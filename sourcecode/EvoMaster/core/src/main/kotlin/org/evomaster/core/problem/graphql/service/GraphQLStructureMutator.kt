package org.evomaster.core.problem.graphql.service

import com.google.inject.Inject
import org.evomaster.core.database.SqlInsertBuilder
import org.evomaster.core.problem.graphql.GraphQLAction
import org.evomaster.core.problem.graphql.GraphQLIndividual
import org.evomaster.core.problem.httpws.service.ApiWsStructureMutator
import org.evomaster.core.problem.rest.SampleType
import org.evomaster.core.search.EvaluatedIndividual
import org.evomaster.core.search.Individual
import org.evomaster.core.search.service.mutator.MutatedGeneSpecification
import org.slf4j.Logger
import org.slf4j.LoggerFactory


/*
    TODO: here there is quite bit of code that is similar to REST.
    Once this is stable, should refactoring to avoid duplication
 */
class GraphQLStructureMutator : ApiWsStructureMutator() {

    companion object{
        private val log: Logger = LoggerFactory.getLogger(GraphQLStructureMutator::class.java)
    }

    @Inject
    private lateinit var sampler: GraphQLSampler


    override fun mutateStructure(individual: Individual, evaluatedIndividual: EvaluatedIndividual<*>, mutatedGenes: MutatedGeneSpecification?, targets: Set<Int>) {

        if (individual !is GraphQLIndividual) {
            throw IllegalArgumentException("Invalid individual type")
        }

        if (!individual.canMutateStructure()) {
            return // nothing to do
        }

        when (individual.sampleType) {
            SampleType.RANDOM -> mutateForRandomType(individual, mutatedGenes)

            //TODO other kinds

            //this would be a bug
            else -> throw IllegalStateException("Cannot handle sample type ${individual.sampleType}")
        }

        if (config.trackingEnabled()) tag(individual, time.evaluatedIndividuals)
    }

    override fun addInitializingActions(individual: EvaluatedIndividual<*>, mutatedGenes: MutatedGeneSpecification?) {
        addInitializingActions(individual, mutatedGenes, sampler)
    }

    private fun mutateForRandomType(ind: GraphQLIndividual, mutatedGenes: MutatedGeneSpecification?) {

        if (ind.seeActions().size == 1) {
            val sampledAction = sampler.sampleRandomAction(0.05) as GraphQLAction

            //save mutated genes
            mutatedGenes?.addRemovedOrAddedByAction(sampledAction, ind.seeActions().size, false, ind.seeActions().size)

            ind.addGQLAction(action= sampledAction)

            return
        }

        if (randomness.nextBoolean() || ind.seeActions().size == config.maxTestSize) {

            //delete one at random
            log.trace("Deleting action from test")
            val chosen = randomness.nextInt(ind.seeActions().size)

            //save mutated genes
            val removedActions = ind.seeActions()[chosen]
            mutatedGenes?.addRemovedOrAddedByAction(removedActions, chosen, true, chosen)

            ind.removeGQLActionAt(chosen)

        } else {

            //add one at random
            log.trace("Adding action to test")
            val sampledAction = sampler.sampleRandomAction(0.05) as GraphQLAction

            val chosen = randomness.nextInt(ind.seeActions().size)
            ind.addGQLAction(chosen, sampledAction)

            //save mutated genes
            mutatedGenes?.addRemovedOrAddedByAction(sampledAction, chosen, false, chosen)
        }

    }

    override fun getSqlInsertBuilder(): SqlInsertBuilder? {
        return sampler.sqlInsertBuilder
    }
}