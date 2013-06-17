SCODEDIR="$ROOTDIR/src/S"
SCODE="control_MZONE.S \
       grid_var_MZONE.X \
       init_var_MZONE.X \
       climat_MZONE.S   \
       density_EOS_MZONE.S \
       output_MZONE.X \
       restart_MZONE.S  \
       realwinds_MZONE.S \
       pmove_MZONE.S"

scode_generate() {
  local MZONE
  local i
  local j
  local FILE
  for i in $*
  do
    MZONE=$i
    for j in $SCODE
    do
      SUBNAME=${j##*.}
      if [ "$SUBNAME" == "S" ]
      then
        FILE=`echo $j|sed "s/MZONE/$MZONE/g"|sed "s/\.S/\.f90/g"`
        sed "s/MZONE/$MZONE/g" $SCODEDIR/$j > $FILE
      elif [ "$SUBNAME" == "X" ]
      then
        FILE=`echo $j|sed "s/MZONE/$MZONE/g"|sed "s/\.X/\.f90/g"`
        check_case
        $ROOTDIR/tools/extract.sh $SCODEDIR/$j $CASE | sed "s/MZONE/$MZONE/g" > $FILE
      else
        echo "I can't recognize the file $SCODEDIR/$j"
        return
      fi
      echo "$FILE generate done"
      SFILES="$SFILES $FILE"
    done
  done
  echo "SFILES = $SFILES" >> ${MDFILE}
}
