#lang forge

option backend smtlibtor


abstract sig Thing {}
abstract sig GeoLocatedObject extends Thing{
	loc :  one GeoLocation
}

// abstract sig MovingObject extends GeoLocatedObject{} -- commented out bc not extended by any children (so it produces warning)

sig GeoLocation extends Thing{
	longitude: one Int,
	latitude: one Int
}

abstract sig Facet extends GeoLocatedObject{
	belongsTo: set Region,
	shareRegion: set Facet
}

abstract sig Region extends GeoLocatedObject{
	contains: one Facet,
	hasLoc: set GeoLocation	
}


one sig f1, f2, f3, f4, f5 extends Facet {}

one sig r1, r2, r3, r4 extends Region {}
one sig l1, l2, l3, l4 extends GeoLocation {}

-- The world model facts
pred model_facts {
  f1.belongsTo = r1
  f1.shareRegion = f2
  
   r1.hasLoc = l1 + l2 + l3

	add[2, l1.longitude] > l1.latitude
	l1.longitude < 29
   l1.latitude > 25
   l1.latitude < 27
  
  f3.belongsTo = r2

  f3.shareRegion = f4

}

pred a {
 f1.shareRegion != f2
}

-- ALLOY-TO-FORGE NOTE: this was originally a failing `check`:
test expect {
    loc_int : {not {model_facts => a}} for 10 is sat
}

