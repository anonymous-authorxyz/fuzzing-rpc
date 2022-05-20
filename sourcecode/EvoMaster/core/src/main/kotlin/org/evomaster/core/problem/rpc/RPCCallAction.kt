package org.evomaster.core.problem.rpc

import org.evomaster.core.problem.api.service.ApiWsAction
import org.evomaster.core.problem.api.service.auth.AuthenticationInfo
import org.evomaster.core.problem.api.service.param.Param
import org.evomaster.core.problem.rpc.auth.RPCAuthenticationInfo
import org.evomaster.core.problem.rpc.auth.RPCNoAuth
import org.evomaster.core.problem.rpc.param.RPCParam
import org.evomaster.core.search.gene.Gene

/**
 * a RPC call
 */
open class RPCCallAction(
    /**
     * id of the RPCCallAction
     */
    val id: String,
    /**
     * a list of input parameters of the action
     */
    inputParameters: MutableList<Param>,
    /**
     * a template of the response of the RPCCall
     * note that the template is immutable, and it is not part of the children of [this]
     */
    val responseTemplate: RPCParam?,
    /**
     * an actual response of the response
     * note that the template is immutable, and it is not part of the children of [this]
     */
    var response : RPCParam?,

    override var auth: RPCAuthenticationInfo = RPCNoAuth()

) : ApiWsAction(auth, inputParameters)  {

    override fun getName(): String {
        return id
    }

    override fun seeGenes(): List<out Gene> {
        // ignore genes in response here
        return parameters.flatMap { it.seeGenes() }
    }

    override fun shouldCountForFitnessEvaluations(): Boolean {
        return true
    }

    override fun copyContent(): RPCCallAction {
        val p = parameters.asSequence().map(Param::copyContent).toMutableList()
        return RPCCallAction(id, p, responseTemplate?.copyContent(), response?.copyContent(), auth)
    }

    /**
     * reset response info
     */
    fun resetResponse() {
        response = null
    }

    /**
     * set no auth for this action
     */
    open fun setNoAuth(){
        auth = RPCNoAuth()
    }
}