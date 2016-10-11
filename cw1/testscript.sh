for i in example*
do
  echo "---- Testing $i ----"
  scala CW1.scala $i > myout.tmp
  scala CW1Solution.jar $i > correctout.tmp
  echo "<diff myout correctout>"
  diff myout.tmp correctout.tmp
  echo "</diff> "
  rm myout.tmp correctout.tmp
done
