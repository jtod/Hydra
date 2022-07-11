// gemsvg - library for figure generators
// This file is part of Hydra.  https://github.com/jtod/Hydra
// Copyright (c) 2022 John T. O'Donnell

import * as fs from "fs"

// Global variables are set by initialize
let fname, fullWidth, fullHeight

//-----------------------------------------------------------------------
// Coordinates and points

// svg coordinates have x increasing to the right, but y increasing
// going down.  These functions convert cartesian coordinates to svg
// coordinates. Coordinates of a point are relative to the frame, not
// to the enclosing box.

export function svgCoordx (x) {
    return number(x)
}

export function svgCoordy (y) {
    return number (fullHeight - y)
}

export function coord (x) {
    return number (x)
}

// A Point defines a position on the canvas

export class Point {
    constructor (x,y) {
        this.x = x
        this.y = y
    }
    draw () {
        return 
    }
    translate (a,b) { // translate by horizontal a, vertical b
        this.x = this.x + a
        this.y = this.y + b
    }
    rotate (c, a) { // rotate by angle a relative to center c
    }
}

//----------------------------------------------------------------------
// Circuit boxes
//----------------------------------------------------------------------

// A circuit box can be a complex graphic with several lines and
// labels, as well as special points (such as ports) that appear at a
// calculated offset from the box reference point.  The boxType
// indicates what the box is (inverter, and2 gate, etc).  The box has
// a reference point with coordinates (refx, refy), and all the parts
// of the box are located relative to the reference point.  The render
// function draws the box.

export class Box {
    constructor (boxType, refx, refy, render) {
        this.boxType = boxType
        this.refx = refx
        this.refy = refy
    }
}

// Edges of a box
export const North = Symbol ('N')
export const East  = Symbol ('E')
export const South = Symbol ('S')
export const West  = Symbol ('W')

// boxType is a string (e.g 'inv' or 'mux1').  The reference point of
// the box is at (refx, refy), which would normally be at the center
// in order to make it easier to keep good horizontal and vertical
// alignment.

// edge is N E S W.  z is offset on the edge. pname is short label
export class Port {
    constructor (edge, z, pname) {
        this.edge = edge
        this.z = z
        this.pname = pname
    }
    // Return coordinates of the point for wire to connect to the port
    connectionPoint () {
        switch (this.edge) {
        case N:
            break
        case East:
            break
        case South:
            break
        case West:
            break
        }
    }
}

//----------------------------------------------------------------------
// Library of components
//----------------------------------------------------------------------

/*
export function defAnd2 () {
    render () {
        let svg = ''
        svg += svgRect (this.refx, this.refy, this.boxw, this.boxh)
        console.log (`box render + ${svg}`)
        return svg 
    }
}
*/
export class Dff {
    constructor (refx, refy) {
    }
}

//-----------------------------------------------------------------------
// Drawing primitive shapes
//-----------------------------------------------------------------------

// Draw line from (x1,y1) to (x2,y2)

export function svgLine (p1, p2) {
    return '<line x1=' + coord(p1.x) + ' y1=' + coord(p1.y)
        + ' x2=' + coord(p2.x) + ' y2=' + coord(p2.y)
        + '/>\n'
}

// Draw a rectangle with center at (x,y) and width w and height h.  In
// svg, you specify the (x,y) coordinates of the upper left corner,
// the width, and the height, and of course the y coordinate counts
// down from the top rather than counting up from the origin.

export function svgRect (cx, cy, w, h) {
    let x = svgCoordx (cx - w/2)
    let y = svgCoordy (cy + h/2)
    return '<rect '
        + ' x=' + x
        + ' y=' + y
        + ' width=' + number(w)
        + ' height=' + number(h) + '\n'
        + '   style="fill: none; stroke: black"/>\n'
}

// A dot is a small filled circle.  In svg you specify a circle with
// coordinates of its center and its radius

export function svgDot (x,y) {
    return '<circle '
        + ' cx=' + svgCoordx(x)
        + ' cy=' + svgCoordy(y)
        + ' r="4"'
        + ' style="fill: black; stroke: black"/>\n'
}

export let bubbleRadius = 4

export function svgBubble (p,direction) {
    let cx = p.x
    let cy = p.y
    let r = bubbleRadius
    if (direction==East) {
        cx += r
    } else {
        cx -= r
    }
    return '<circle '
        + ' cx=' + coord(cx)
        + ' cy=' + coord(cy)
        + ' r=' + coord(r)
        + ' style="fill: none; stroke: black"/>\n'
}

export function svgText (x,y,t) {
    console.log ('svgtext')
    let xs = '<text '
        + ' x=' + svgCoordx(x)
        + ' y=' + svgCoordy(y)
        + ' style="font-family:sans-serif; font-size:14pt,fill:black">'
        + t
        + '</text>\n'
    console.log (xs)
    return xs
}

//-----------------------------------------------------------------------
// The figure generator program must first call initialize
//-----------------------------------------------------------------------

export function initialize (figname, description, w, h) {
    console.log (`initialize ${figname}`)
    fullWidth = w
    fullHeight = h
    fname = figname
    let xs = '// ' + figname + ' - ' + description + '\n'
        + '// generated ' + '\n'
    xs = ''
    fs.writeFileSync (`${fname}.svg`, xs)
    prologue (description)
    boundingBox ()
}

export function boundingBox () {
    // south boundary
    outsvg ('<line x1=' + coord(0)
            + ' y1=' + coord(0)
            + ' x2=' + coord(fullWidth)
            + ' y2=' + coord(0)
            + ' style="stroke: green"'
            + '/>\n')
    // west boundary
    outsvg ('<line x1=' + coord(0)
            + ' y1=' + coord(0)
            + ' x2=' + coord(0)
            + ' y2=' + coord(fullHeight)
            + ' style="stroke: green"'
            + '/>\n')
    // north bounday
    outsvg ('<line x1=' + coord(0)
            + ' y1=' + coord(fullHeight)
            + ' x2=' + coord(fullWidth)
            + ' y2=' + coord(fullHeight)
            + ' style="stroke: green"'
            + '/>\n')
    // east bounday
    outsvg ('<line x1=' + coord(fullWidth)
            + ' y1=' + coord(0)
            + ' x2=' + coord(fullWidth)
            + ' y2=' + coord(fullHeight)
            + ' style="stroke: green"'
            + '/>\n')
}


//----------------------------------------------------------------------
// Beginning and end of svg file
//----------------------------------------------------------------------

export function mkPrologue (fname, description) {
    return '<svg'
        + ' width=' + number(fullWidth)
        + ' height=' + number(fullHeight) + '\n'
        + '    xmlns="http://www.w3.org/2000/svg"\n'
        + '    xmlns:xlink="http://www.w3.org/1999/xlink">\n'
        + '  <title>' + fname + '</title>\n'
        + '  <desc>' + description + '</desc>\n'
//        + '  <g style="font-size: 11pt; font-family: sans-serif">\n'

}
      
export function prologue (description) {
    fs.appendFileSync (`${fname}.svg`, mkPrologue (fname, description))
}

export function epilogue () {
    fs.appendFileSync (`${fname}.svg`, '</svg>\n')
}


export function mkLine (p1,p2) { outsvg (svgLine (p1,p2)) }
export function mkBubble (p,dir) { outsvg (svgBubble (p,dir)) }

export function mkRectSvg (a,b,c,d) { outsvg (svgRect (a,b,c,d)) }
export function mkDotSvg (a,b) { outsvg (svgDot (a,b)) }
export function mkTextSvg (a,b,t) { outsvg (svgText (a,b,t)) }


//----------------------------------------------------------------------
// Generating definitions
//----------------------------------------------------------------------

export function startDefs () {
    outsvg (`<defs>`)
}
export function finishDefs () {
    outsvg (`</defs>`)
}

export function startComponent () {
    outsvg (`<g>`)
}
export function finishComponent () {
    outsvg (`</g>`)
}

export function generateComponents () {
    startDefs ()
    defAnd2 ()
    endDefs ()
}

//----------------------------------------------------------------------
// Utilities
//----------------------------------------------------------------------

export function outsvg (txt) {
    fs.appendFileSync (`${fname}.svg`, txt)
}

// return string containing n
export function number (n) {
    return '"' + n + '"'
}

