package org.evomaster.core.problem.rest

import io.swagger.parser.OpenAPIParser
import org.evomaster.core.EMConfig
import org.evomaster.core.problem.rest.param.BodyParam
import org.evomaster.core.problem.rest.param.FormParam
import org.evomaster.core.problem.rest.resource.ResourceCluster
import org.evomaster.core.problem.util.ParamUtil
import org.evomaster.core.search.Action
import org.evomaster.core.search.gene.*
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Disabled
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource

class RestActionBuilderV3Test{

    @Test
    fun testEnumYml(){

        val map = loadAndAssertActions("/swagger/artificial/openapi-enum.yml", 2)
        val get = map["GET:/api/enum"]!!
        val post = map["POST:/api/enum"]!!

        val getEnums = get.seeGenes().flatMap { it.flatView() }.filterIsInstance<EnumGene<*>>()
        assertEquals(2, getEnums.size)
        val postEnums = post.seeGenes().flatMap { it.flatView() }.filterIsInstance<EnumGene<*>>()
        assertEquals(1, postEnums.size)
    }

    @Test
    fun testParseDto(){

        val name = "com.FooBar"
        val foo = "foo"
        val bar = "bar"

        val dtoSchema = """
            "$name": {
                 "type": "object",
                 "properties": {
                        "$foo": { 
                            "type": "string"
                        },
                        "$bar": {
                            "type": "integer"
                        }
                 },
                 "required": [
                    "$foo"
                 ]
            }     
        """.trimIndent()

        val gene = RestActionBuilderV3.createObjectGeneForDTO(name, dtoSchema, name) as ObjectGene
        assertEquals(name, gene.name)
        assertEquals(2, gene.fields.size)

        val str = gene.fields.find { it is StringGene } as StringGene
        assertEquals(foo, str.name)

        val nr = gene.fields.find { it is OptionalGene } as OptionalGene
        assertEquals(bar, nr.name)
    }




    //---------------------------------

    private fun loadAndAssertActions(resourcePath: String, expectedNumberOfActions: Int)
            : MutableMap<String, Action> {


        val schema = OpenAPIParser().readLocation(resourcePath, null, null).openAPI

        val actions: MutableMap<String, Action> = mutableMapOf()

        RestActionBuilderV3.addActionsFromSwagger(schema, actions)

        assertEquals(expectedNumberOfActions, actions.size)

        return actions
    }

    private fun checkNumOfRootGene(actionCluster: Map<String, Action>,
                                   skipActions: List<String>,
                                   expectedNumberOfActions: Int,
                                   expectedNumOfRootGene: Int,
                                   expectedNumOfIG0: Int,
                                   expectedNumOfIGM: Int,
                                   expectedNumOfObjOfIGM: Int){


        val cluster = actionCluster.filterNot { skipActions.contains(it.key.split(":")[1]) }
        assertEquals(expectedNumberOfActions, cluster.size)

        var numOfRG = 0
        var numOfIG0 = 0
        var numOfIGM = 0
        var numOfObjOfIGM = 0

        cluster.values.forEach { a->
            numOfRG += a.seeGenes().size
            numOfIG0 += a.seeGenes().count { g-> g.innerGene().isEmpty() }
            numOfIGM += a.seeGenes().count { g-> g.innerGene().isNotEmpty() }
            numOfObjOfIGM += a.seeGenes().count { g-> ParamUtil.getValueGene(g) is ObjectGene }
        }
        assertEquals(expectedNumOfRootGene, numOfRG)
        assertEquals(expectedNumOfIG0, numOfIG0)
        assertEquals(expectedNumOfIGM, numOfIGM)
        assertEquals(expectedNumOfObjOfIGM, numOfObjOfIGM)
    }

    private fun checkNumResource(actionCluster: Map<String, Action>, skipActions: List<String>, numOfResource: Int, numOfIndResource: Int){
        val manipulated  = actionCluster.filterNot { skipActions.contains(it.key.split(":")[1]) }

        val cluster = ResourceCluster()
        val config = EMConfig()
        config.doesApplyNameMatching = true
        cluster.initResourceCluster(manipulated, config = config)

        assertEquals(numOfResource, cluster.getCluster().size)
        assertEquals(numOfIndResource, cluster.getCluster().count { it.value.isIndependent() })
    }

    // ----------- V3 --------------

    @Test
    fun testNexmo(){
        loadAndAssertActions("/swagger/apisguru-v3/nexmo.json", 5)
    }

    @Test
    fun testBcgnews() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/bcgnws.json", 14)
    }

    @Test
    fun testBclaws() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/bclaws.json", 7)
    }

    @Test
    fun testBng2latlong() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/bng2latlong.json", 1)
    }

    @Test
    fun testChecker() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/checker.json", 1)
    }

    @Test
    fun testDisposable() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/disposable.json", 1)
    }

    @Test
    fun testFraudDetection() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/fraud-detection.json", 2)
    }

    @Test
    fun testGeolocation() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/geolocation.json", 1)
    }

    @Test
    fun testIp2proxy() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/ip2proxy.com.json", 1)
    }

    @Test
    fun testApisGuruNews() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/news.json", 27)
    }

    @Test
    fun testOpen511() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/open511.json", 4)
    }

    @Test
    fun testSmsVerification() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/sms-verification.json", 2)
    }

    @Test
    fun testValidation() {
        val map = loadAndAssertActions("/swagger/apisguru-v3/validation.json", 1)
    }



    // ----------- V2 --------------

    @Test
    fun testGitLab() {
        loadAndAssertActions("/swagger/others/gitlab.json", 358)
    }

    @Test
    fun testCyclotron() {
        val map = loadAndAssertActions("/swagger/sut/cyclotron.json", 50)
        checkNumOfRootGene(map, listOf(),50, 87, 16, 71, 11)
        checkNumResource(map, listOf(), 40, 18)
    }


    @Test
    fun testPetStore() {
        loadAndAssertActions("/swagger/others/petstore.json", 20)
    }


    @Test
    fun testMultiParamPath() {
        loadAndAssertActions("/swagger/artificial/multi_param_path.json", 1)
    }


    @Test
    fun testNcs() {
        val map = loadAndAssertActions("/swagger/sut/ncs.json", 6)
        checkNumOfRootGene(map, listOf(),6, 14, 0, 14, 0)
        checkNumResource(map, listOf(), 6, 6)

    }

    @Test
    fun testScs() {
        val map = loadAndAssertActions("/swagger/sut/scs.json", 11)
        checkNumOfRootGene(map, listOf(),11, 26, 0, 26, 0)
        checkNumResource(map, listOf(), 11, 11)

    }

    @Test
    fun testGestaoHospital() {
        val map = loadAndAssertActions("/swagger/sut/gestaohospital.json", 20)
        checkNumOfRootGene(map, listOf(),20, 43, 14, 29, 6)
        checkNumResource(map, listOf(), 13, 0)

    }

    @Test
    fun testDisease() {
        val map = loadAndAssertActions("/swagger/sut/disease_sh_api.json", 34)
        checkNumOfRootGene(map, listOf(),34, 57, 0, 57, 0)
        checkNumResource(map, listOf(), 34, 34)
    }

    @Test
    fun testRealWorld() {
        val map = loadAndAssertActions("/swagger/sut/realworld_app.json", 19)
        checkNumOfRootGene(map, listOf(),19, 31, 6, 25, 6)
        checkNumResource(map, listOf(), 11, 2)
    }

    @Test
    fun testSpaceX() {
        val map = loadAndAssertActions("/swagger/sut/spacex_api.json", 94)
        checkNumOfRootGene(map, listOf(),94, 102, 29, 73, 29)
        checkNumResource(map, listOf(), 52, 5)
    }



    @Test
    fun testNews() {
        val map = loadAndAssertActions("/swagger/sut/news.json", 7)

        val create = map["POST:/news"] as RestCallAction
        assertEquals(2, create.seeGenes().size)
        val bodyNews = create.seeGenes().find { it.name == "body" }
        assertNotNull(bodyNews)
        assertNotNull(bodyNews is OptionalGene)
        assertNotNull((bodyNews as OptionalGene).gene is ObjectGene)
        assertNotNull((bodyNews.gene as ObjectGene).refType)
        assertEquals("NewsDto", (bodyNews.gene as ObjectGene).refType)

        checkNumOfRootGene(map, listOf(),7, 12, 3, 9, 2)
        checkNumResource(map, listOf(), 4, 1)

    }


    @Test
    fun testCatWatch() {
        val map = loadAndAssertActions("/swagger/sut/catwatch.json", 23)

        val postScoring = map["POST:/config/scoring.project"] as RestCallAction
        assertEquals(3, postScoring.seeGenes().size)
        val bodyPostScoring = postScoring.seeGenes().find { it.name == "body" }
        assertNotNull(bodyPostScoring)
        assertTrue(bodyPostScoring is OptionalGene)
        assertTrue((bodyPostScoring as OptionalGene).gene is StringGene)

        val skipInEM = listOf("/fetch", "/health", "/health.json", "/error")
        checkNumOfRootGene(map,skipInEM ,13, 36, 4, 32, 1)
        checkNumResource(map, skipInEM, 13, 11)
    }

    @Test
    fun testProxyPrint() {

        //TODO check  Map<String, String> in additionalProperties

        val map = loadAndAssertActions("/swagger/sut/proxyprint.json", 115)

        val balance = map["GET:/consumer/balance"] as RestCallAction
        //Principal should not appear, because anyway it is a GET
        assertTrue(balance.parameters.none { it is BodyParam })


        val update = map["PUT:/consumer/info/update"] as RestCallAction
        //   Type is JSON, but no body info besides wrong Principal
        assertTrue(update.parameters.none { it is BodyParam })


        val register = map["POST:/consumer/register"] as RestCallAction
        // Same for WebRequest
        assertTrue(register.parameters.none { it is BodyParam })

        val skipInEM = listOf(
            "/heapdump", "/heapdump.json",
            "/autoconfig", "/autoconfig.json",
            "/beans", "/beans.json",
            "/configprops", "/configprops.json",
            "/dump", "/dump.json",
            "/env", "/env.json", "/env/{name}",
            "/error",
            "/health", "/health.json",
            "/info", "/info.json",
            "/mappings", "/mappings.json",
            "/metrics", "/metrics.json", "/metrics/{name}",
            "/trace", "/trace.json"
        )
        checkNumOfRootGene(map, skipInEM, 74, 82,22, 60, 14)

        checkNumResource(map, skipInEM, 56, 26)

    }

    @Test
    fun testCreateActions() {
        loadAndAssertActions("/swagger/artificial/positive_integer_swagger.json", 2)
    }

    @Test
    fun testSchemaWithErrorEndpoint() {
        loadAndAssertActions("/swagger/artificial/positive_integer_swagger_errors.json", 1)
    }


    @Test
    fun testOCVN() {
        val map = loadAndAssertActions("/swagger/sut/ocvn_1oc.json", 192)
        checkNumOfRootGene(map, listOf(),192, 2852, 0, 2852, 0)
        checkNumResource(map, listOf(), 96, 0)

    }

    @Disabled("This is a bug in Swagger Core, reported at https://github.com/swagger-api/swagger-core/issues/2100")
    @Test
    fun testFeaturesServicesNull() {
        loadAndAssertActions("/swagger/sut/features_service_null.json", 18)
    }

    @Test
    fun testFeaturesServices() {
        val map = loadAndAssertActions("/swagger/sut/features_service.json", 18)
        checkNumOfRootGene(map, listOf(),18, 37, 4, 33, 4)
        checkNumResource(map, listOf(), 11, 1)
    }

    @Test
    fun testScoutApi() {
        val map = loadAndAssertActions("/swagger/sut/scout-api.json", 49)
        checkNumOfRootGene(map, listOf(),49, 127, 19, 108, 19)
        checkNumResource(map, listOf(), 21, 2)
    }

    @Test
    fun testLanguageTool(){
        val map = loadAndAssertActions("/swagger/sut/languagetool.json", 2)
        checkNumOfRootGene(map, listOf(),2, 2, 1, 1, 1)
        checkNumResource(map, listOf(), 2, 1)
    }

    @Test
    fun testRestCountries(){
        val map = loadAndAssertActions("/swagger/sut/restcountries.yaml", 22)
        checkNumOfRootGene(map, listOf(),22, 34, 2, 32, 0)
        checkNumResource(map, listOf(), 22, 22)
    }

    @Test
    fun testCwaVerification(){
        val map = loadAndAssertActions("/swagger/sut/cwa_verification.json", 5)
        checkNumOfRootGene(map, listOf(),5, 12, 4, 8, 5)
        checkNumResource(map, listOf(), 5, 0)
    }


    @Test
    fun testBranches() {
        loadAndAssertActions("/swagger/artificial/branches.json", 3)
    }



    //TODO need to handle "multipart/form-data"
    @Disabled
    @Test
    fun testSimpleForm() {
        val actions = loadAndAssertActions("/swagger/artificial/simpleform.json", 1)

        val a = actions.values.first() as RestCallAction

        assertEquals(HttpVerb.POST, a.verb)
        assertEquals(2, a.parameters.size)
        assertEquals(2, a.parameters.filter { p -> p is FormParam }.size)
    }

    @Test
    fun testDuplicatedParamsInFeaturesServices() {
        val actions = loadAndAssertActions("/swagger/sut/features_service.json", 18)
        (actions["POST:/products/{productName}/configurations/{configurationName}/features/{featureName}"] as RestCallAction).apply {
            assertEquals(3, parameters.size)
        }
    }


    @Test
    fun testApisGuru() {

        val actions = loadAndAssertActions("/swagger/apisguru-v2/apis.guru.json", 2)

        actions.values
                .filterIsInstance<RestCallAction>()
                .forEach {
                    assertEquals(2, it.produces.size)
                    assertTrue(it.produces.any{ p -> p.contains("application/json")})
                }
    }

    @Test
    fun testGreenPeace() {
        loadAndAssertActions("/swagger/apisguru-v2/greenpeace.org.json", 6)
    }


    @Test
    fun testRestApiExample(){
        val resourcePath = "/swagger/others/rest-api-example.json"
        val actions = loadAndAssertActions(resourcePath, 3)

        val get = actions["GET:/api/items"]
        assertNotNull(get)

        val schema = OpenAPIParser().readLocation(resourcePath, null, null).openAPI
        val map = mutableMapOf<String,ObjectGene>()
        RestActionBuilderV3.getModelsFromSwagger(schema, map)

        assertEquals(3, map.size)
        val x = map["Iterable«Item»"] as ObjectGene //this is due to bug in SpringFox that does not handle Iterable<T>
        assertEquals(0, x.fields.size)
    }


    @ParameterizedTest
    @ValueSource(strings = ["/swagger/artificial/reference_type_v2.json","/swagger/artificial/reference_type_v3.json"])
    fun testReferenceType(path: String){
        val actions = loadAndAssertActions(path, 1)
        val bodyParam = actions.values.filterIsInstance<RestCallAction>().flatMap { it.parameters }.filterIsInstance<BodyParam>()
        assertEquals(1, bodyParam.size)
        assertTrue(bodyParam.first().gene is ObjectGene)
        (bodyParam.first().gene as ObjectGene).apply {
            assertNotNull(refType)
            assertEquals("Component", refType)
            val info = (fields.find { it.name == "info" } as? OptionalGene)
            assertNotNull(info)
            assertTrue((info as OptionalGene).gene is ObjectGene)
            (info.gene as ObjectGene).apply {
                assertNotNull(refType)
                assertEquals("Info", refType)
                val at = (fields.find { it.name =="at" } as? OptionalGene)
                assertNotNull(at)
                assertTrue((at as OptionalGene).gene is ArrayGene<*>)
                (at.gene as ArrayGene<*>).apply {
                    assertTrue(template is ObjectGene)
                    assertEquals("AT", (template as ObjectGene).refType)
                }
            }

        }
    }
}