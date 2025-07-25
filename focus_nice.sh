THRESHOLD=15
FAST=-20 #priority niceness lvl
SLOW=19 #low priority
DELAY=0.5

IFS="
"

echo "Starting 'Nice Focus'";
while true; do
        top_out=$(top -b -n 1 | grep "name of process")
        for i in $top_out; do
                cpu_usage=$(echo -n "$i" | sed 's/\s\s*/ /g' | cut -d' ' -f8 | cut -d . -f 1)
		
		echo -n "PROCESS: {top_out} --> CPU USAGE: {cpu_usage}";
                echo $i
                if [ "$cpu_usage" -gt "$THRESHOLD" ]; then
                        pid=$(echo -n "$i" | cut -b 1-5)
                        sudo renice -n "$FAST" -p "$pid" 
		fi
                if [ "$cpu_usage" -lt "$THRESHOLD" ]; then
                        pid=$(echo -n "$i" | cut -b 1-5)
                        sudo renice -n "$SLOW" -p "$pid" 
                fi
        done
        
        sleep $DELAY
done
