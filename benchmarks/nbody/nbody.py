import math

PI = 3.141592653589793
SOLAR_MASS = 4 * PI * PI
DAYS_PER_YEAR = 365.24

class Body:
    def __init__(self, x, y, z, vx, vy, vz, mass):
        self.x = x
        self.y = y
        self.z = z
        self.vx = vx
        self.vy = vy
        self.vz = vz
        self.mass = mass

def advance(bodies, dt):
    n = len(bodies)
    for i in range(n):
        for j in range(i + 1, n):
            dx = bodies[i].x - bodies[j].x
            dy = bodies[i].y - bodies[j].y
            dz = bodies[i].z - bodies[j].z

            dSquared = dx*dx + dy*dy + dz*dz
            distance = math.sqrt(dSquared)
            mag = dt / (dSquared * distance)

            bodies[i].vx -= dx * bodies[j].mass * mag
            bodies[i].vy -= dy * bodies[j].mass * mag
            bodies[i].vz -= dz * bodies[j].mass * mag

            bodies[j].vx += dx * bodies[i].mass * mag
            bodies[j].vy += dy * bodies[i].mass * mag
            bodies[j].vz += dz * bodies[i].mass * mag

    for i in range(n):
        bodies[i].x += dt * bodies[i].vx
        bodies[i].y += dt * bodies[i].vy
        bodies[i].z += dt * bodies[i].vz

def energy(bodies):
    e = 0.0
    n = len(bodies)
    for i in range(n):
        e += 0.5 * bodies[i].mass * (bodies[i].vx**2 + bodies[i].vy**2 + bodies[i].vz**2)
        for j in range(i + 1, n):
            dx = bodies[i].x - bodies[j].x
            dy = bodies[i].y - bodies[j].y
            dz = bodies[i].z - bodies[j].z
            distance = math.sqrt(dx*dx + dy*dy + dz*dz)
            e -= (bodies[i].mass * bodies[j].mass) / distance
    return e

def offset_momentum(bodies):
    px = py = pz = 0.0
    for b in bodies:
        px += b.vx * b.mass
        py += b.vy * b.mass
        pz += b.vz * b.mass
    bodies[0].vx = -px / SOLAR_MASS
    bodies[0].vy = -py / SOLAR_MASS
    bodies[0].vz = -pz / SOLAR_MASS

def main():
    bodies = [
        # Sun
        Body(0, 0, 0, 0, 0, 0, SOLAR_MASS),
        # Jupiter
        Body(4.84143144246472090e+00, -1.16032004402742839e+00, -1.03622044471123109e-01,
             1.66007664274403694e-03 * DAYS_PER_YEAR, 7.69901118419740425e-03 * DAYS_PER_YEAR, -6.90460016972063023e-05 * DAYS_PER_YEAR,
             9.54791938424326609e-04 * SOLAR_MASS),
        # Saturn
        Body(8.34336671824457987e+00, 4.12479856412430479e+00, -4.03523417114321381e-01,
             -2.76742510726862411e-03 * DAYS_PER_YEAR, 4.99852801234917238e-03 * DAYS_PER_YEAR, 2.30417297573763929e-05 * DAYS_PER_YEAR,
             2.85885980666130812e-04 * SOLAR_MASS),
        # Uranus
        Body(1.28943695621391310e+01, -1.51111514016986312e+01, -2.23307578892655734e-01,
             2.96460137564761618e-03 * DAYS_PER_YEAR, 2.37847173959480950e-03 * DAYS_PER_YEAR, -2.96589568540237556e-05 * DAYS_PER_YEAR,
             4.36624404335156298e-05 * SOLAR_MASS),
        # Neptune
        Body(1.53796971148509165e+01, -2.59193146099879641e+01, 1.79258772950371181e-01,
             2.68067772490389322e-03 * DAYS_PER_YEAR, 1.62824170038242295e-03 * DAYS_PER_YEAR, -9.51592254519715870e-05 * DAYS_PER_YEAR,
             5.15138902046611451e-05 * SOLAR_MASS),
    ]

    offset_momentum(bodies)
    print(f"{energy(bodies):.9f}")

    n = 170000
    for _ in range(n):
        advance(bodies, 0.01)

    print(f"{energy(bodies):.9f}")

if __name__ == "__main__":
    main()
