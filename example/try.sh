#!/bin/bash 

TIME_BUDGET="1s"

if [[ $# -eq 0 ]] ; then
    echo "Default 1s"
    else
      echo "Time budget: $1";
      TIME_BUDGET=$1
fi

echo "Starting SUT"

java -jar  -Xms1G -Xmx4G -Dem.muteSUT=true -Devomaster.instrumentation.jar.path=evomaster-agent.jar -jar rpc-thrift-ncs-evomaster-runner.jar  30010 30011 rpc-thrift-ncs-sut.jar 1200 >>sut-log.txt 2>&1 &
CONTROLLER_PID=$! 

sleep 10

echo "Start to run EvoMaster for RPC-based APIs"
echo

java -jar -Xms2G -Xmx4G  -jar evomaster.jar  --algorithm=MIO --enableRPCExtraResponseTargets=True --enablePureRPCTestGeneration=True --enableRPCAssertionWithInstance=True --testSuiteFileName=EM_RPC_1_Test --maxTime=$TIME_BUDGET --statisticsColumnId=rpc-thrift-ncs --seed=1 --sutControllerPort=30010  --snapshotInterval=5 --writeStatistics=true


kill $CONTROLLER_PID
