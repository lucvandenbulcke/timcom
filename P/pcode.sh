PCODEDIR="$ROOTDIR/src/P"

pcode_generate() {
  local i
  local PFILES
  MYDIR=$PWD
  if [ -d "$PCODEDIR" ]
  then
    cd $PCODEDIR
    for i in `ls *.f90`
    do
      cp $i $MYDIR/ 
      echo "$i generate done"
      PFILES="$PFILES $i"
    done
    cd $MYDIR
    echo "PFILES = $PFILES" >> ${MDFILE}
  else
    echo "$PCODEDIR directory does NOT exist!"
  fi
}
