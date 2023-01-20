#lang froglet
option run_sterling off

abstract sig EndPoint {}
sig Server extends EndPoint { causes: set HTTPEvent }
sig Client extends EndPoint {}

abstract sig HTTPEvent {
  from: set EndPoint,
  to: set EndPoint,
  origin: set EndPoint
}

sig Request extends HTTPEvent {
  response: lone Response
}

sig Response extends HTTPEvent {
  embeds: set Request
}

pred Acyclic [r: Request]{
  r not in r.^(response.embeds)
}


