# White-box Fuzzing RPC-based APIs: An Industrial Case Study


In this package, we provide necessary info for replicating experiment in the paper.

- [suts](suts) contains jars files for two artificial case studies (i.e., _thrif-ncs_ and _thrif-scs_) 
- [tools](tools) contains  our tool (i.e., [evomaster.jar](replication/evomaster.jar)) built with jar. Note that the jar could also be built with provided source code.
- [rpc-exp.py](rpc-exp.py) is a python script used for conducting the experiment.
- [sourcecode](sourcecode) contains source code of our approach ([RPC-EVO](sourcecode/EvoMaster) built on the top of [EvoMaster](https://www.evomaster.org/)) and artificial case studies developed with [Apache Thrift](https://thrift.apache.org/) ([RPC-based APIs](sourcecode/RpcAPIs))
- [rawdata](rawdata) contains raw experiment results on the two artificial case studies.
- [r-script](r-script) provides R-scripts for analyzing results.
- [generated_files](generated_files) are a set of tables and figures that could be generated with the [r-script](r-script) based on the [rawdata](rawdata).

---
**NOTE**

Due to confidential problem in industrial case studies, we only provide info of artificial case studies in this replication package.

---

### Environment Setup
Our tool is built on the top of [EvoMaster](https://www.evomaster.org/) which support JDK 8 and JDK 17 (the major LTS versions now).

### Fuzzing RPC-based API

#### Try
To try our tool, we provide a bash script to run it on the _thrift-ncs_ RPC-based APIs.

Go to [exmaple](example) dir, then run `try.sh` with a specified time budget, eg, 1s as a following example.
> `./try.sh 1s`

After it finishes, you could get 
- [src/em](replication/src/em): generated tests, e.g.,
```
public class EM_RPC_1_Test_others {

    private static org.thrift.ncs.client.NcsService.Client var_client0_NcsService_Iface; // client
    
    
    
    @Test(timeout = 60000)
    public void test_0() throws Exception {
        
        
        org.thrift.ncs.client.Dto res_0 = null;
        {
         int arg0 = 695;
         int arg1 = 5;
         res_0 = var_client0_NcsService_Iface.remainder(arg0,arg1);
        }
        assertEquals(0, res_0.resultAsInt);
        assertTrue(numbersMatch(0.0, res_0.resultAsDouble));
    }
}

```
- _statistics.csv_: statistics info for this run.

#### Step by step
There exist two simple steps to start the tool,

Step 1. Start the driver which handles the SUT. The driver could be _embedded_ (see an [embedded driver](sourcecode/RpcAPIs/em/embedded/thrift/ncs/src/main/java/em/embedded/org/thriftncs/EmbeddedEvoMasterController.java) for rpc-ncs) or _external_ (see an [external driver](sourcecode/RpcAPIs/em/external/thrift/ncs/src/main/java/em/external/org/rpc/thriftncs/ExternalEvoMasterController.java)). Both could be started with IDE. 

Step 2. Run the tool with specified configruation, e.g., time budget
> `java -jar evomaster.jar --maxTime=1s`

all available configurations could be found with `java -jar evomaster.jar --help`


### Replicate the experiment
We provide a python script which could generate bash scripts for running the experiment, with `<cluster> <baseSeed> <dir> <minSeed> <maxSeed> <maxActions> <minutesPerRun> <nJobs>`.
To replicate our experiment (100 000 RPC function calls with 30 repetitions), it would take around 30.64 hours using 
> `python rpc-exp.py false 12345 rpc-exp 1 30 100000 400 100`.

To try our tool, you could run once (i.e., Random and MIO algorithm) with 10 000 RPC function calls on the two case studies using
> `python rpc-exp.py false 12345 rpc-exp 1 1 10000 400 2`.


With the command, `rpc-exp` folder should be created as the following structure
```
rpc-exp/
├─ logs/
│  ├─ evomaster/
├─ reports/
├─ scripts/
│  ├─ evomaster_12355_rpc-thrift-ncs.sh
│  ├─ ...
├─ tests/
├─ runall.sh
├─ evomaster.jar
├─ ...
```

To start the experiment, go to `rpc-exp` as the example, then run
> `./runall.sh`

After the experiment is done, under `rpc-exp`
- `reports` would contain statistics files (`.csv` format), e.g., a number of covered targets;
- `tests` would contain generated tests with JUnit (`.java`).

