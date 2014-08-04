@echo off

echo Qsim

java -jar -Djava.system.class.loader=com.uqbar.apo.APOClassLoader QSim.jar

pause
@echo on 