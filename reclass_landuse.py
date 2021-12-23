# -*- coding: utf-8 -*-
"""
Created on Thu Dec 23 14:54:11 2021

@author: Guopeng.Jiang
"""

# Pre-logic script code
def reclass(a):
  if a in ["Orchard","Vineyard"]:
    a = "Orchard | Vineyard"
  else:
    a = a
  return a

reclass( !Landuse! ) 