OCODEDIR="$ROOTDIR/src/O"

ocode_generate() {
  local i
  local OFILES
  MYDIR=$PWD
  if [ -d "$OCODEDIR" ]
  then
    cd $OCODEDIR
    for i in `ls *.f90`
    do
      cp $i $MYDIR/ 
      echo "$i generate done"
      OFILES="$OFILES $i"
    done
    cd $MYDIR
    echo "OFILES = $OFILES" >> ${MDFILE}
  else
    echo "$OCODEDIR directory does NOT exist!"
  fi
}
