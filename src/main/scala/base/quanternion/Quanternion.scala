package engine
package base.quanternion

import base.RawVector

def plusQuanternion(l: RawVector, r: RawVector) : RawVector = {
    RawVector(l.x + r.x, r.y + r.y, l.z + r.z, l.a + r.a)
}

def + = plusQuanternion

def multiQuanternion(l: RawVector, r: RawVector): RawVector = {
    RawVector(
        l.y * r.z - l.z * r.y + l.x * r.a + l.a * r.x,
        -l.x * r.z + l.z * r.x + l.y * r.a + l.a * r.y,
        l.x * r.y - l.y * r.x + l.z * r.a + l.a * r.z,
        - l.x * r.x - l.y * r.y - l.z * r.z + l.a * r.a
    )
}

def ** = multiQuanternion

def crossQuanternion(l: RawVector, r: RawVector) : RawVector = {
    RawVector(
        l.x * r.x,
        l.y * r.y,
        l.z * r.z,
        l.a * r.a
    )
}

def * = crossQuanternion