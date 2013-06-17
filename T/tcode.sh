TCODEDIR="$ROOTDIR/src/T"
TCODE=( "SZONE2DZONE.T" \
       "DZONE2SZONE.T" )

tcode_generate() {
  local MODEL
  local NZONE
  local SZONE
  local DZONE
  local FILE
  local i
  local j
  local k
  j=0
  for i in $*
  do
    j=$((j+1))
    MODEL[j]=$i
  done
  NZONE=$j
  if [ $NZONE -gt 1 ]; then
    for ((i=1;i<=$[NZONE-1];i++))
    do
      j=$((i+1))
      SZONE=${MODEL[i]}
      DZONE=${MODEL[j]}
      for k in ${TCODE[@]}
      do
        FILE=`echo $k|sed "s/SZONE/$SZONE/g"|sed "s/DZONE/$DZONE/g"|sed "s/\.T/\.f90/g"`
        if [ "$k" == "${TCODE[0]}" ];then
          $ROOTDIR/tools/extract.sh $TCODEDIR/$k $DZONE | sed "s/SZONE/$SZONE/g" | sed "s/DZONE/$DZONE/g" >& $FILE
        elif [ "$k" == "${TCODE[1]}" ];then
          $ROOTDIR/tools/extract.sh $TCODEDIR/$k $SZONE | sed "s/SZONE/$SZONE/g" | sed "s/DZONE/$DZONE/g" >& $FILE
        fi
        echo "$FILE generate done"
        TFILES="$TFILES $FILE"
      done
    done
    echo "TFILES = $TFILES" >> ${MDFILE}
  fi

}
