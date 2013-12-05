cd ../QSim
mvn install
cd ../QSim_UI
mvn assembly:assembly -DdescriptorId=jar-with-dependencies
mv target/QSim.jar build/

