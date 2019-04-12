#!/usr/bin/awk -f

BEGIN {
    my_n = 0
    var["Slow"] = "Siso"
    var["Linear"] = "Linear"
}

(/ID 10[0-3][0-9]/) {
    id=$5;
    type=$8;
    time=($NF+0);
    my_n=0;
    ids[id] = id
    movements[id, time] = type

}

(/time to complete/) {
    my_n = my_n + 1;
#    printf("add %d place %d for id %d (type %s) x = %s\n", $9, my_n, id, type, x)
    timings[id, time, my_n] = $9
#    print id, my_n, timings[id, my_n]
}

END {
    printf("id\ttime\ttype\ttime1\ttime2\ttime3\ttime4\ttime5\ttime6\n")
    for(id in ids) {
	for (time = 0; time < 4; time++) {
	    type = movements[id, time]
	    printf("%d\t%d\t%s", id, time+1, type);
	    for (x = 1; x <= 6; x++) {
		printf("\t%s", timings[id, time, x]);
	    }
	    printf("\n")
	}
    }
}
