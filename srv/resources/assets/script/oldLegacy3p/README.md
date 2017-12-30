# About this

* `date/` — is very old package, not distributed anywhere, supposed to be
   replaced with `moment` package later
* `myJQuery.coffee` — own **jquery** dependency resolver, to use
   **jquery-migrate** package instead, also to require **jquery-migrate** with
   injecting original **jquery** dependency (not using it right now)
   Removed dependencies from `package.json`:
     * `"jquery-migrate": "^3.0.1"`
* `jquery-2.2.4.js` — because cannot include older version via npm with new
   node.js version, see description about 'd3' library below for more details.
* `d3-v3.5.17.js` — Placed as built bundle because can't use **d3** dependency
   with nodejs version *&gt;=8.x*, in dependency tree **contextify** package
   appears, it cannot be build with new node's API, authors of **contextify**
   are not going to fix this, they recommend to update dependency to newer
   version that would be *4.x* for **d3**, this major update breaks our code.
