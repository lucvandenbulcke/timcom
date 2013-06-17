MCODEDIR="$ROOTDIR/src/M"
MCODEHEAD="physics input initfs fs timcom"

mcode_generate()  {
  local MFILES
  local MASTR
  local SLAVE
  local NZONE
  local MZONE
  local i
  local j
  local k

for mh in $MCODEHEAD
do

  local MASTR_FILE=${mh}_MASTR.X
  local SLAVE_FILE=${mh}_SLAVE.M

  j=0
  for i in $*
  do
    j=$((j+1))
    MZONE[j]=$i
  done
  NZONE=$j
  MASTR=${MZONE[1]}
  FILE=`echo $MASTR_FILE|sed "s/MASTR/$MASTR/g"|sed "s/\.X/\.f90/g"`
  check_case
  $ROOTDIR/tools/extract.sh $MCODEDIR/$MASTR_FILE $CASE | sed "s/MASTR/$MASTR/g" >& $FILE
  echo "$FILE generate done"  
  MFILES="$MFILES $FILE"
  if [ $NZONE -gt 1 ]; then
    for ((j=2;j<=$NZONE;j++))
    do
       i=$((j-1))
       MASTR=${MZONE[i]}
       SLAVE=${MZONE[j]}
       FILE=`echo $SLAVE_FILE|sed "s/SLAVE/$SLAVE/g"|sed "s/\.M/\.f90/g"`
       sed "s/SLAVE/$SLAVE/g" $MCODEDIR/$SLAVE_FILE | sed "s/MASTR/$MASTR/g" >& $FILE
       echo "$FILE generate done"
       MFILES="$MFILES $FILE"
    done
  fi

done
  echo "MFILES = $MFILES" >> ${MDFILE} 
}

