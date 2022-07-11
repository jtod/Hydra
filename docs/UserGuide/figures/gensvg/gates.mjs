// gates.mjs - test gensvg
// This file is part of Hydra.  https://github.com/jtod/Hydra
// Copyright (c) 2022 John T. O'Donnell

// usage: node gates.mjs, writes file gates.mjs.svg

import * as tools from './gensvg.mjs'
import * as path from 'path'
import { fileURLToPath } from 'url'

const figname = path.basename (fileURLToPath(import.meta.url))
console.log (`basename = ${figname}`)

// Set properties and initialize the figure
let description = 'logic gates'
const fullWidth = 410
const fullHeight = 310

function inv () {
    // origin (reference point) is f at left edge where input comes in
    console.log ("Generate inverter layout")
    let stublen = 15
    let invH = 25
    let invW = 35
    let a = new tools.Point (stublen, invH)
    let b = new tools.Point (stublen, 0)
    let c = new tools.Point (stublen+invW, invH/2)
    let e = new tools.Point (0, invH/2)
    let f = new tools.Point (stublen, invH/2)
    let g = new tools.Point (c.x + 8, invH/2)
    let h = new tools.Point (g.x + stublen, invH/2)
    tools.outsvg ('<defs>\n')
    tools.outsvg ('<g id="inv" style="stroke:black">\n')
    tools.mkLine (a,b)
    tools.mkLine (a,c)
    tools.mkLine (b,c)
    tools.mkLine (e,f)
    tools.mkBubble (c,tools.East)
    tools.mkLine (g,h)
    tools.outsvg ('</g>\n')
    tools.outsvg ('</defs>\n')
    }




// Generate figure
tools.initialize (figname, description, fullWidth, fullHeight)
inv ()

tools.outsvg ('<text x="200" y="250">special</text>\n')
tools.outsvg ('<use xlink:href="#inv" x="50" y="90" />\n')
tools.outsvg ('<use xlink:href="#inv" x="200" y="200" />\n')
tools.epilogue ()

/*
    tools.outsvg ('<text x="10" y="20">qwe</text>\n')
    let vb = ' viewBox="' // not using
        + "0"
        + ' ' + "0"
        + ' ' + (invW+2*stublen)
        + ' ' + invH
        + '"'
*/

//    console.log (`vb=${vb}`)
//    tools.outsvg ('<symbol id="inv" style="stroke:red"' + vb + '>\n')
    //    tools.outsvg ('<text x="20" y="15">qwe</text>\n')
//    tools.outsvg ('</symbol>\n')
