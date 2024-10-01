#lang forge

option verbose 0
option backend smtlibtor

one sig Helper {
  anInt: one Int,
  someInts: set Int
}

test expect {
    {sum[Helper.anInt] = 1} is sat
    {sum[Helper.someInts] = 1} is forge_error
    // Checker doesn't do anything more refined for +
    {sum[Helper.anInt + Helper.anInt] = 1} is forge_error
    {sum[Helper.anInt & Helper.anInt] = 1} is sat
    {sum[Helper.someInts & Helper.someInts] = 1} is forge_error
    {Helper.anInt = 1} is sat
    {Helper.someInts = 1} is sat
}


