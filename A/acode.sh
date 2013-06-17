ACODEDIR="$ROOTDIR/src/A"

ACODE="grids.f90 \
       date.f90 \
       ufileformat.f90 \
       regreg.f90 \
       getm_meteo.f90 \
       initfile.f90 \
       interfext.f90 "

acode_generate() {
  local i
  local AFILES
  local CFILES
  MYDIR=$PWD
  if [ -d "$ACODEDIR" ]
  then
    cd $ACODEDIR
    for i in $ACODE
    do
      cp $i $MYDIR/ 
      echo "$i generate done"
      AFILES="$AFILES $i"
    done
    for i in `ls *.c`
    do
      cp $i $MYDIR/
      echo "$i generate done"
      CFILES="$CFILES $i"
    done
    for i in `ls *.h`
    do
      cp $i $MYDIR/
      echo "$i copy done"
    done
    cd $MYDIR
    echo "AFILES = $AFILES" >> ${MDFILE}
    echo "CFILES = $CFILES" >> ${MDFILE}
  else
    echo "$PCODEDIR directory does NOT exist!"
  fi
}
