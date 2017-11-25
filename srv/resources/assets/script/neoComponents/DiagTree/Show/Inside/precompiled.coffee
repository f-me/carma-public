# WARNING! This module will be precompiled.
# See for details: srv/webpackLoaders/precompile-code-loader.coffee

# h ∈ [0..360]; s ∈ [0..1]; v ∈ [0..1]
# see https://github.com/tmpvar/hsv2rgb/blob/master/hsv2rgb.js
# returns list of 3 elements: [r, g, b]
hsv2rgb = (h, s, v) ->
  h = h % 360
  s = Math.max 0, Math.min(s, 1)
  v = Math.max 0, Math.min(v, 1)

  return (Math.round x * 255 for x in [v, v, v]) unless s

  b  = ((1 - s) * v)
  vb = v - b
  hm = h % 60

  Math.round x * 255 for x in \
    switch (h / 60) | 0
      when 0 then [v, vb * h / 60 + b, b]
      when 1 then [vb * (60 - hm) / 60 + b, v, b]
      when 2 then [b, v, vb * hm / 60 + b]
      when 3 then [b, vb * (60 - hm) / 60 + b, v]
      when 4 then [vb * hm / 60 + b, b, v]
      when 5 then [v, b, vb * (60 - hm) / 60 + b]

backgrounds =
  (backgroundColor: "rgb(#{r}, #{g}, #{b})" for [r, g, b] in \
    (hsv2rgb x / 7 * 360, 0.10, 1 for x in [0..6]))

module.exports = {backgrounds}
