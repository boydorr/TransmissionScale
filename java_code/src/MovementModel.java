import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import jdistlib.*;
import jdistlib.rng.RandomEngine;

/**
 * Created by rebeccamancy on 03/05/2015.
 */
public class MovementModel {

    private Metapopulation mp;
    private Weibull spatialKernelDistribution;
    private Gamma handlingTimeDistribution;
    private Uniform directionDistribution;
    private Exponential discoveryTimeDistribution;
    private double longDistProb; private String longDistanceLocationsType;
    private GeometryFactory factory = new GeometryFactory();
    private double TdMean, ThShape, ThScale;
    private double cellAreaSqKm;
    private RandomEngine random;
    private String movementModelName;
    private String additionalString;

    /**
     *
     * @param mp Metapopulation object
     * @param movementModelName Name of the movement model
     * @param skShape Shape parameter of spatial kernel
     * @param skScale Scale parameter of spatial kernel
     * @param longDistProb Probability of long-distance movement
     * @param TdMean Mean search (or "discovery") time
     * @param ThShape Shape parameter of handling time
     * @param ThScale Scale parameter of handling time
     * @param additionalString To pass additional information about type of run (see class Initialise for details)
     * @param cellAreaSqKm Spatial scale information
     * @param longDistanceLocationsType How to simulate long-distance movement (e.g. to grid cells proportional to dog density)
     * @param random Random number generator
     */
    public MovementModel(Metapopulation mp, String movementModelName, double skShape, double skScale, double longDistProb, double TdMean, double ThShape, double ThScale, String additionalString,
                         double cellAreaSqKm, String longDistanceLocationsType, RandomEngine random) {

        this.mp = mp;
        this.movementModelName = movementModelName;
        this.random = random;

        // Spatial kernel distributions - random angle and Gamma distance
        spatialKernelDistribution = new Weibull(skShape, skScale);
        spatialKernelDistribution.setRandomEngine(random);
        // The angle is computed anti-clockwise, starting from the x-axis
        // To facilitate conversion, we parameterise this as running from (-pi, +pi) in radians
        directionDistribution = new Uniform(0, 2*Math.PI);
        directionDistribution.setRandomEngine(random);

        // Probability of long-distance dispersal (e.g. human mediated transport of dogs)
        this.longDistProb = longDistProb;

        // FD and DD parameters - discovery time, handling time, abandonment time
        this.TdMean = TdMean;
        this.ThShape = ThShape;
        this.ThScale = ThScale;
        this.additionalString = additionalString;
        this.cellAreaSqKm = cellAreaSqKm;
        this.longDistanceLocationsType = longDistanceLocationsType;

        // Handling time distribution (Gamma)
        handlingTimeDistribution = new Gamma(ThShape, ThScale);
        handlingTimeDistribution.setRandomEngine(random);

    }

    /**
     * Get search time Td, given handling time Th; Td and Th are drawn for each new case
     * @param Th
     * @return
     */
    public double getRandTd(double Th) {
        if (movementModelName.equals("fixedDensityDependence")) {
            double ThMean = ThShape * ThScale;
             double ratio = Th/ThMean; // ratio of actual Th to ThMean
            return(TdMean * ratio);
        } else if (movementModelName.equals("freeDensityDependence")) {
            return TdMean;
        } else {
            System.out.println("movementModelName needs to be fixedDensityDependence or freeDensityDependence");
            System.exit(0);
            return 0;
        }
    }

    /**
     * Draw a random handling time Th; this simulates between-dog heterogeneity in parameters (while holding density dependence constant)
     * @return
     */
    public double getRandTh() {
        // return (ThScale * ThShape); // Fixed Th, Td
        return handlingTimeDistribution.random();
    }

    /**
     * Use average handling time Th for all dogs; used when not allowing between-dog heterogeneity
     * @return
     */
    public double getFixedTh() {
        // System.out.println("Mv > getFixedTh: " + ThScale + "  " + ThShape);
        return (ThScale * ThShape); // Fixed Th, Td

    }

    /**
     * Determines type of movement (long- or short-distance) and generates Point location of bite
     * Does the check to ensure generated point is within StudyArea
     * @param startLocation Initial location prior to this movement
     * @param probablisticLongDistance Boolean that says whether to to implement probabilistic long-distance movement
     * @return Point object of new location
     */
    public Point randNewLocation(Point startLocation, boolean probablisticLongDistance) {
        Point newLocation = null;

        // If we are not excluding long-distance movement, test against threshold probability
        if ( probablisticLongDistance && random.nextDouble() < longDistProb ) {
            // Long-distance movement
            while (mp.findPopulation(newLocation)==null) { // Keep going until within the metapopulation
                newLocation = mp.generateRandomPoint(longDistanceLocationsType);
            }
        } else {
            // Short-distance: Get a movement vector and generate a new point from this
            while (mp.findPopulation(newLocation)==null) { // Keep going until within the metapopulation
                Coordinate vec = randGammaMovementVector();
                newLocation = factory.createPoint(new Coordinate(startLocation.getX() + vec.x, startLocation.getY() + vec.y));
            }
        }
        /// System.out.println("Movement from (" + startLocation.toString() + ") to (" + newLocation + ")");
        return newLocation;
    }

    /**
     * Helper method generates a movement vector (uses Coordinate rather than Point as is meant to be a vector!)
     * @return
     */
    public Coordinate randGammaMovementVector() {

        // Random distance and direction
        double dist = spatialKernelDistribution.random();
        //System.out.println("MovementModel>randGammaMvtVector Distance travelled: " + dist);
        double angle = directionDistribution.random();

        // Convert to a vector
        double X = Math.cos(angle) * dist; // Opposite
        double Y = Math.sin(angle) * dist; // Adjacent

        return new Coordinate(X,Y);
    }

    /**
     * Simulate a new "Excursion" or "Foray" (see class below)
     * @param startLocation
     * @param t
     * @param TEnd
     * @param Td
     * @param Th
     * @param probablisticLongDistance
     * @return
     */
    public Excursion getNewExcursion(Point startLocation, double t, double TEnd, double Td, double Th, boolean probablisticLongDistance) {
        Excursion e = new Excursion(startLocation, mp, t, TEnd, Td, Th, cellAreaSqKm, probablisticLongDistance);
        return e;
    }

    /**
     * Class Excursion - for each excursion, we have a location, population in which it takes place, id of dog bitten, and duration (Te)
     */
    public class Excursion {
        private Point location;
        private Population pop;
        private int dogID = -1; // NB: use -1 if no dog found
        private double Te;  // Duration of this excursion

        /**
         *
         * @param startLocation Point for the initial location of this excursion
         * @param mp The metapopulation we're using
         * @param t Elapsed time into the infectious period
         * @param TEnd Time of death
         * @param Td Discovery time
         * @param Th Handling time
         * @param cellAreaSqKm Area of cell(s) in square km
         */
        private Excursion(Point startLocation, Metapopulation mp, double t, double TEnd, double Td, double Th, double cellAreaSqKm, boolean probablisticLongDistance) {

            // Set a location for the excursion and allocate a population to this
            this.location = randNewLocation(startLocation, probablisticLongDistance);
            this.pop = mp.findPopulation(this.location);

            if(this.pop == null) {
                System.out.println("Looks like I didn't find this population");
            }

            Te = simExcursionDuration(pop, Th, Td, cellAreaSqKm); // Draw from an exponential with rate (rc) given by Holling curve
            // Assumes that a bite happens only if the whole of the handling time occurs (bites occur at the end of the handling time)
            if (pop.getTotalDogs() > 0) {  // Only make further actions if there are dogs in this population (else do nothing, case 1)
                // If bite occurs before rabid dog dies, select dog at random to bite i.e. if the excursion lasts less time than the remainder of the infectious period
                if ((Te + t) < TEnd) {
                    this.dogID = random.nextInt(pop.getTotalDogs()); // Choose a dog at random to bite, ids from 0 to nDogs-1 : random.nextInt is in [0, n)
                }
            } // else were no dogs in grid cell, so nothing happens

        }

        // Getter methods and toString
        public double getDuration() { return (this.Te); }
        public Population getPop() { return this.pop; }
        public int getDogID(){ return this.dogID; }
        public Point getLocation() { return this.location; }
        public String toString() { return "dogID " + dogID + ", Te = " + Te + " at " + location.toString() + " in " + pop.toString() ; }
    }

    /**
     * Computes an encounter rate, for normal excursions in the simulator (NB: the other version below is used in ABCOutcomes)
     * @param N
     * @param Th
     * @param Td
     * @param cellAreaSqKm
     * @return
     */
    public double computeExcursionRate ( int N, double Th, double Td, double cellAreaSqKm ) {

        // System.out.println("In MovementModel > computeExcursionRate, and cellAreaSqKm = " + cellAreaSqKm); // Check units
        // Compute excursion rate, adding one dog per sq km to avoid getting infinite excursion durations (this also avoids the need for a separate "giving up time")
        double density = 1 + (N/cellAreaSqKm); // density per square km

        // Excursion "rate" within this cell (used to make an excursion duration) - searching, plus handling if N>0
        double rc = (density/Td) / (1 + ((Th*density)/Td) );

        return rc * 1.0;
    }

    /**
     * Computes an excursion rate. NB: Static version used from ABCOutcomes, where we don't have a movement model
     * @param density True density - one is added within the function
     * @param Th
     * @param Td
     * @return
     */
    public static double computeExcursionRate ( double density, double Th, double Td ) {

        // Compute excursion rate, adding one dog per sq km to avoid getting infinite excursion durations (this also avoids the need for a separate "giving up time")
        density = density + 1; // density per square km

        // Excursion "rate" within this cell (used to make an excursion duration) - searching, plus handling if N>0
        double rc = (density/Td) / (1 + ((Th*density)/Td) );

        return rc;
    }

    /**
     * Returns an encounter duration drawn from an Exponential distribution with mean (r_c) based on the population, cell density, and a given handling and discovery rate
     * (The Th and Td are drawn from a distribution, but this does not happen inside this function)
     * @param p
     * @param Th
     * @param Td
     * @return
     */
    public double simExcursionDuration (Population p, double Th, double Td, double cellAreaSqKm) {

        // Compute excursion rate, adding one dog per sq km to avoid getting infinite excursion durations (this also avoids the need for a separate "giving up time")
        int N = p.getTotalDogs();

        // Excursion "rate" within this cell (used to make an excursion duration) - searching, plus handling
        double rc = computeExcursionRate(N, Th, Td, cellAreaSqKm);

        // Simulate an excursion duration based on rate
        return Util.randExp(rc, random);
    }

    public String getAdditionalString() {
        return this.additionalString;
    }

}

