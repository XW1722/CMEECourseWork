"""
This script runs both LV1.py and LV2.py with certain arguments.
In this script, it also profiles the two scripts.
The results are printed.
""" 
import LV1
import LV2
LV1.main()
LV2.main([1, 10, 0.5, 0.3, 3000])