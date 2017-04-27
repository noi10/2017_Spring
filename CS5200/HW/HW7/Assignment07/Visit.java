package Assignment07;

/**
 * A visit by a safari to a city.
 */
public class Visit {
    /** The day of the visit within the safari. */
    private int dayOfVisit = 1;

    /** The city being visited. */
    private City city = null;

    /**
     * Construct a visit by a safari to a city.
     * @param dayOfVisit The day of the visit.
     * @param city The city being visited.
     */
    public Visit(int dayOfVisit, City city) {
        this.dayOfVisit = dayOfVisit;
        this.city = city;
    }

    /**
     * Get the day of the visit within the safari.
     * @return the day of the visit within the safari.
     */
    public int getDayOfVisit() {
        return dayOfVisit;
    }

    /**
     * Get the city being visited.
     * @return the city being visited.
     */
    public City getCity() {
        return city;
    }
}
