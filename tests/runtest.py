#!/Library/Frameworks/Python.framework/Versions/3.7/bin/python3
import csv
import subprocess

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

alltot = 0
allfail = 0
testfolder = "./"
positive = "./positive.csv"
negative= "./negative.csv"
tool = "../Checker"

def runtest(myfile, fun, outcome,a,b,c):
    with open(myfile,'r') as f:
        testreader = csv.reader(f)
        tot = 0
        fail = 0
        unk = 0
        for row in testreader:
        
            sub = testfolder + (row[a].strip())
            if (row[0].strip()) != "%":
                sup = testfolder + (row[b].strip())
                print("Testing: ",tool, sub, sup)
                cmd = subprocess.Popen([tool,sub, sup], stdout=subprocess.PIPE)
                try:
                    cmd.wait(timeout=2)
                    currentlist = []
                    tot +=1
                    for line in cmd.stdout:
                        ret = line.strip().decode("utf-8")
                        if ret == "True":
                            currentlist = [True]+currentlist
                        if ret == "False":
                            currentlist = [False]+currentlist
                        if ret == "Maybe":
                            unk +=1
                            print(bcolors.WARNING + "\tUnknown!" + bcolors.ENDC)
                    if currentlist == []:
                        currentlist = [outcome]
                    else:
                        if fun(currentlist)!=outcome:
                            fail +=1
                            print(bcolors.WARNING)
                            print("\t Unexpected outcome for:")
                            print("\t ../Checker", sub, sup, row[c],"+RTS -N2")
                            print("\t The tool says",fun(currentlist),"and we expected",outcome)
                            print(bcolors.ENDC)
                except subprocess.TimeoutExpired:
                    cmd.kill()
                    cmd.wait()
                    print(bcolors.WARNING)
                    print("\t Checker timedout:")
                    print("\t ../Checker", sub, sup, row[c],"+RTS -N2")
                    print(bcolors.ENDC)
        print(bcolors.BOLD + "Failed:",(fail),"/ Unknown:",(unk))
        print("Summary: ",(tot-(fail+unk)),"/",tot," passed."+bcolors.ENDC)
        
        return (tot,fail,unk)



print("----------------POSITIVE----------------")
(t1,f1,u1) = runtest(positive, all, True, 0,1,2)

print("----------------NEGATIVE----------------")
(t2,f2,u2) = runtest(negative, all, False, 0,1,2)

print(bcolors.BOLD+"----------------------------------")
print("Failed:",(f1+f2),"/ Unknown:",(u1+u2))
print("Overall summary: ",(t1+t2)-(f1+f2+u1+u2),"/",(t1+t2)," passed.")
print("----------------------------------"+bcolors.ENDC)

