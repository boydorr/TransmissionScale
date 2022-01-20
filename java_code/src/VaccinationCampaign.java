/**
 * Created by rebeccamancy on 30/08/2015.
 */
public class VaccinationCampaign implements Comparable<VaccinationCampaign> {
    private static final String COMMA_DELIMITER = ", ";

    private int popID, id;       // id is used to order vaccinations when several on same day
    private String auName;
    private int doses;
    private double dayVaccinations;

    /**
     * Constructs a VaccinationCampaign object, consisting of a number of doses in a population on a specified date (and a campaign id)
     * Constructed as part of the creation of a metapopulation object
     * @param auName Administrative unit name (not required)
     * @param dayVaccinations day on which vaccinations took place (double)
     * @param popID ID of the population (grid cell) in which these took place
     * @param doses Number of doses delivered
     * @param id ID of vaccination campaign (used for ordering when several on the same day)
     */
    public VaccinationCampaign(String auName, double dayVaccinations, int popID, int doses, int id) {
        this.auName = auName;
        this.dayVaccinations = dayVaccinations;
        this.popID = popID;
        this.doses = doses;
        this.id = id;
    }

    // Getters, setters, and toString
    public double getDayVaccinations() {
        return dayVaccinations;
    }

    public String getAuName() { return auName; }

    public int getDoses() { return doses; }

    public int getPopID() {
        return popID;
    }

    public int compareTo(final VaccinationCampaign other) {
        if (other == null) {
            return 1;
        }
        final int comp = Double.compare(this.dayVaccinations, other.dayVaccinations);
        if (comp == 0) {  // i.e. if same day
            return this.id - other.id;
        } else {
            return comp;
        }
    }

    public String toString() {
        return "popID:" + popID + COMMA_DELIMITER + " auName:" + auName + COMMA_DELIMITER + " day:" + dayVaccinations + COMMA_DELIMITER + " doses:" + doses;
    }
}
