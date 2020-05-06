#!/bin/bash

PS3="番号で選択して下さい: "
select fruit in apple orange banana grape exit
do
	  case $fruit in
		    apple|orange|banana|grape)
			      echo "$fruit が選択されました!"
			      ;;
		    exit)
			      break
			      ;;
		    *)
			      echo "1 から 4 の番号で選んで下さい."
			      ;;
	  esac
done
