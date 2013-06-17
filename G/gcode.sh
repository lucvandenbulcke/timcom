
init_code_generate() {
  local MZONE
  local NZONE
  local SZONE
  local DZONE
  local FILE
  local i
  local j
  FILE=init.f90
  rm -rf $FILE 
  echo "SUBROUTINE INIT" >> $FILE
  echo "USE TIMCOM_GENERAL" >> $FILE
  echo "USE OCN_PARA" >> $FILE
  j=0
  for i in $*
  do
    j=$((j+1))
    MZONE[j]=$i
  done
  NZONE=$j
  if [ $NZONE -gt 1 ]; then
    for ((i=1;i<=$[NZONE-1];i++))
    do
      j=$((i+1))
      SZONE=${MZONE[i]}
      DZONE=${MZONE[j]}
      echo "USE ${SZONE}2${DZONE}" >> $FILE
      echo "USE ${DZONE}2${SZONE}" >> $FILE
    done
  fi
  echo "CALL INITFS" >> $FILE
  echo "CALL INITATMO" >> $FILE
  for i in $*
  do
    #echo "CALL INITFS_${i}(I0_$i,J0_$i,K0_$i,N0_$i,NBIR_$i)" >> $FILE
    echo "CALL INITFS_${i}(I0_$i,J0_$i,K0_$i)" >> $FILE
  done
  echo "CALL INITATMO_FREE" >> $FILE
  if [ $NZONE -gt 1 ]; then
    for ((i=1;i<=$[NZONE-1];i++))
    do
      j=$((i+1))
      SZONE=${MZONE[i]}
      DZONE=${MZONE[j]}
      echo "CALL ${SZONE}2${DZONE}_INIT" >> $FILE
      echo "CALL ${DZONE}2${SZONE}_INIT" >> $FILE
#      echo "CALL ${SZONE}${DZONE}_INIT" >> $FILE
    done
 fi
 echo "END SUBROUTINE INIT" >> $FILE
 echo "$FILE generate done"
 GFILES="$GFILES $FILE"
}

ocn_para_mod_generate() {
  local FILE
  FILE=ocn_para.f90
  rm -rf $FILE
  echo "MODULE OCN_PARA" >> $FILE
  for i in $*
  do
    echo "INTEGER :: I0_$i , J0_$i , K0_$i" >> $FILE
  done
  for i in $*
  do
    echo "NAMELIST /${i}/ I0_$i , J0_$i , K0_$i" >> $FILE
  done
  echo "END MODULE OCN_PARA" >> $FILE
  echo "$FILE generate done"
  GFILES="$GFILES $FILE"
}

input_code_generate() {
  local FILE
  FILE=input.f90
  rm -rf $FILE
  echo "MODULE INPUT" >> $FILE
  echo "USE TIMCOM_GENERAL" >> $FILE
  echo "USE OCN_PARA" >> $FILE
  echo "USE date" >> $FILE
  echo "USE initfile" >> $FILE
  echo "CONTAINS" >> $FILE
  echo "SUBROUTINE READ_CASE_INFO" >> $FILE
  echo 'OPEN(20,FILE=".case_info",CONVERT="BIG_ENDIAN",STATUS="OLD",FORM="UNFORMATTED")' >> $FILE
  echo "READ(20)CASE_NAME" >> $FILE
  echo "READ(20)DSCRIB" >> $FILE
  echo "CLOSE(20)" >> $FILE 
  echo "END SUBROUTINE READ_CASE_INFO" >> $FILE
  echo "" >> $FILE

  echo "SUBROUTINE READ_CASE_SIZE" >> $FILE
  for i in $*
  do
    echo 'OPEN(20,FILE="./INPUT_'${i}'/'${i}'",CONVERT="BIG_ENDIAN",STATUS="OLD",FORM="UNFORMATTED")' >> $FILE
    echo "READ(20)I0_${i},J0_${i},K0_${i}" >> $FILE
    echo "CLOSE(20)" >> $FILE
  done
  echo "END SUBROUTINE READ_CASE_SIZE" >> $FILE
  echo "" >> $FILE

  echo "SUBROUTINE READ_NAMELIST_INPUT" >> $FILE
  echo "CHARACTER(12)::tempname" >> $FILE
  echo 'write(tempname,"(A,i3.3,A)") "run",member,".init"' >> $FILE
  echo 'write(*,*) "reading general configuration: ",tempname' >> $FILE

  echo "call getInitValue(tempname,'WORKDIR',WORKDIR)" >> $FILE
  echo "call getInitValue(tempname,'RUNS',RUNS)" >> $FILE
  echo "call getInitValue(tempname,'ISAV',ISAV)" >> $FILE
  echo "call getInitValue(tempname,'VAR_I0J0K1_OUTPUT',VAR_I0J0K1_OUTPUT)" >> $FILE
  echo "call getInitValue(tempname,'VAR_I0J0K1_HDF5',VAR_I0J0K1_HDF5)" >> $FILE
  echo "call getInitValue(tempname,'UVAR_OUTPUT',UVAR_OUTPUT)" >> $FILE
  echo "call getInitValue(tempname,'VVAR_OUTPUT',VVAR_OUTPUT)" >> $FILE
  echo "call getInitValue(tempname,'SVAR_OUTPUT',SVAR_OUTPUT)" >> $FILE
  echo "call getInitValue(tempname,'TVAR_OUTPUT',TVAR_OUTPUT)" >> $FILE
  echo "call getInitValue(tempname,'PVAR_OUTPUT',PVAR_OUTPUT)" >> $FILE
  echo "call getInitValue(tempname,'VAR_I0J0K0_OUTPUT',VAR_I0J0K0_OUTPUT)" >> $FILE
  echo "call getInitValue(tempname,'WVAR_OUTPUT',WVAR_OUTPUT)" >> $FILE
  echo "call getInitValue(tempname,'SCRNOUT',SCRNOUT)" >> $FILE
  echo "call getInitValue(tempname,'CPSAV',CPSAV)" >> $FILE
  echo "call getInitValue(tempname,'ATMOSFILE',ATMOSFILE)" >> $FILE
  echo "call getInitValue(tempname,'t0year',t0year)" >> $FILE
  echo "call getInitValue(tempname,'t0month',t0month)" >> $FILE
  echo "call getInitValue(tempname,'t0day',t0day)" >> $FILE
  echo "call getInitValue(tempname,'t0hour',t0hour)" >> $FILE
  echo "call getInitValue(tempname,'t0minut',t0minut)" >> $FILE
  echo "t0mjd=mjd2(t0year,t0month,t0day,t0hour,t0minut)" >> $FILE

  echo "END SUBROUTINE READ_NAMELIST_INPUT" >> $FILE
  echo "END MODULE INPUT" >> $FILE
  echo "$FILE generate done"
  GFILES="$GFILES $FILE"
}

timcom_code_generate() {
  local MZONE
  local NZONE
  local FILE
  local i
  local j
  FILE=timcom.f90
  rm -rf $FILE
  j=0
  for i in $*
  do
    j=$((j+1))
    MZONE[j]=$i
  done
  NZONE=$j
  echo "SUBROUTINE TIMCOM" >> $FILE
  if [ $NZONE -gt 1 ]; then

    for ((i=1;i<=$[NZONE-1];i++))
    do
      j=$((i+1))
      SZONE=${MZONE[i]}
      DZONE=${MZONE[j]}
      echo "USE ${SZONE}2${DZONE}" >> $FILE
      echo "USE ${DZONE}2${SZONE}" >> $FILE
    done
    for ((i=1;i<=$NZONE;i++))
    do
      j=$((i+1))
      if [ "$j" -gt "$NZONE" ]; then
        j=$((i-1))
      fi
      SZONE=${MZONE[i]}
      DZONE=${MZONE[j]}
      echo "CALL TIMCOM_${MZONE[i]}" >> $FILE
      echo "CALL ${SZONE}2${DZONE}_COUPLE_OUT" >> $FILE
      echo "CALL ${SZONE}2${DZONE}_COUPLE_IN" >> $FILE
    done

    for ((j=$[NZONE-1];j>=2;j--))
    do
      i=$((j-1))
      SZONE=${MZONE[i]}
      DZONE=${MZONE[j]}
      echo "CALL ${DZONE}2${SZONE}_COUPLE_OUT" >> $FILE
      echo "CALL ${DZONE}2${SZONE}_COUPLE_IN" >> $FILE
    done
  else
    echo "CALL TIMCOM_${MZONE[1]}" >> $FILE
  fi
  echo "END SUBROUTINE TIMCOM" >> $FILE
  echo "$FILE generate done"
  GFILES="$GFILES $FILE"
}

gcode_generate() {
  ocn_para_mod_generate $DOMAINLIST
  init_code_generate $DOMAINLIST
  input_code_generate $DOMAINLIST
  timcom_code_generate $DOMAINLIST
echo "GFILES = ${GFILES}" >> ${MDFILE}
}

