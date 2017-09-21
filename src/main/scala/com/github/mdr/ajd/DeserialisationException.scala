package com.github.mdr.ajd

class DeserialisationException(message: String, causeOpt: Option[Throwable] = None)
  extends RuntimeException(message, causeOpt.orNull)
