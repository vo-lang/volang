public class Nbody {
    static final double PI = 3.141592653589793;
    static final double SOLAR_MASS = 4 * PI * PI;
    static final double DAYS_PER_YEAR = 365.24;

    static class Body {
        double x, y, z;
        double vx, vy, vz;
        double mass;

        Body(double x, double y, double z, double vx, double vy, double vz, double mass) {
            this.x = x; this.y = y; this.z = z;
            this.vx = vx; this.vy = vy; this.vz = vz;
            this.mass = mass;
        }
    }

    static void advance(Body[] bodies, double dt) {
        int n = bodies.length;
        for (int i = 0; i < n; i++) {
            for (int j = i + 1; j < n; j++) {
                double dx = bodies[i].x - bodies[j].x;
                double dy = bodies[i].y - bodies[j].y;
                double dz = bodies[i].z - bodies[j].z;

                double dSquared = dx*dx + dy*dy + dz*dz;
                double distance = Math.sqrt(dSquared);
                double mag = dt / (dSquared * distance);

                bodies[i].vx -= dx * bodies[j].mass * mag;
                bodies[i].vy -= dy * bodies[j].mass * mag;
                bodies[i].vz -= dz * bodies[j].mass * mag;

                bodies[j].vx += dx * bodies[i].mass * mag;
                bodies[j].vy += dy * bodies[i].mass * mag;
                bodies[j].vz += dz * bodies[i].mass * mag;
            }
        }

        for (int i = 0; i < n; i++) {
            bodies[i].x += dt * bodies[i].vx;
            bodies[i].y += dt * bodies[i].vy;
            bodies[i].z += dt * bodies[i].vz;
        }
    }

    static double energy(Body[] bodies) {
        double e = 0.0;
        int n = bodies.length;
        for (int i = 0; i < n; i++) {
            e += 0.5 * bodies[i].mass * (bodies[i].vx*bodies[i].vx + bodies[i].vy*bodies[i].vy + bodies[i].vz*bodies[i].vz);
            for (int j = i + 1; j < n; j++) {
                double dx = bodies[i].x - bodies[j].x;
                double dy = bodies[i].y - bodies[j].y;
                double dz = bodies[i].z - bodies[j].z;
                double distance = Math.sqrt(dx*dx + dy*dy + dz*dz);
                e -= (bodies[i].mass * bodies[j].mass) / distance;
            }
        }
        return e;
    }

    static void offsetMomentum(Body[] bodies) {
        double px = 0, py = 0, pz = 0;
        for (Body b : bodies) {
            px += b.vx * b.mass;
            py += b.vy * b.mass;
            pz += b.vz * b.mass;
        }
        bodies[0].vx = -px / SOLAR_MASS;
        bodies[0].vy = -py / SOLAR_MASS;
        bodies[0].vz = -pz / SOLAR_MASS;
    }

    public static void main(String[] args) {
        Body[] bodies = {
            // Sun
            new Body(0, 0, 0, 0, 0, 0, SOLAR_MASS),
            // Jupiter
            new Body(4.84143144246472090e+00, -1.16032004402742839e+00, -1.03622044471123109e-01,
                1.66007664274403694e-03 * DAYS_PER_YEAR, 7.69901118419740425e-03 * DAYS_PER_YEAR, -6.90460016972063023e-05 * DAYS_PER_YEAR,
                9.54791938424326609e-04 * SOLAR_MASS),
            // Saturn
            new Body(8.34336671824457987e+00, 4.12479856412430479e+00, -4.03523417114321381e-01,
                -2.76742510726862411e-03 * DAYS_PER_YEAR, 4.99852801234917238e-03 * DAYS_PER_YEAR, 2.30417297573763929e-05 * DAYS_PER_YEAR,
                2.85885980666130812e-04 * SOLAR_MASS),
            // Uranus
            new Body(1.28943695621391310e+01, -1.51111514016986312e+01, -2.23307578892655734e-01,
                2.96460137564761618e-03 * DAYS_PER_YEAR, 2.37847173959480950e-03 * DAYS_PER_YEAR, -2.96589568540237556e-05 * DAYS_PER_YEAR,
                4.36624404335156298e-05 * SOLAR_MASS),
            // Neptune
            new Body(1.53796971148509165e+01, -2.59193146099879641e+01, 1.79258772950371181e-01,
                2.68067772490389322e-03 * DAYS_PER_YEAR, 1.62824170038242295e-03 * DAYS_PER_YEAR, -9.51592254519715870e-05 * DAYS_PER_YEAR,
                5.15138902046611451e-05 * SOLAR_MASS),
        };

        offsetMomentum(bodies);
        System.out.printf("%.9f%n", energy(bodies));

        int n = 150000;
        for (int i = 0; i < n; i++) {
            advance(bodies, 0.01);
        }

        System.out.printf("%.9f%n", energy(bodies));
    }
}
