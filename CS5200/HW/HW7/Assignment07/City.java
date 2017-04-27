package Assignment07;

/**
 * A city in a country.
 */
public class City {
    /** The identifier of the city. */
    private int id = 0;

    /** The name of the city. */
    private String name = null;

    /** The country that the city is a part of. */
    private Country partOf = null;

    /**
     * Construct a city in a country.
     * @param id The city identifier.
     * @param name The name of the city.
     * @param partOf The country that the city is a part of.
     */
    public City(int id, String name, Country partOf) {
	this.id = id;
	this.name = name;
	this.partOf = partOf;
    }

    /**
     * Get the identifier of the city.
     * @return the city identifier in the database.
     */
    public int getId() {
	return id;
    }

    /**
     * Get the name of the city.
     * @return the name of the city.
     */
    public String getName() {
	return name;
    }

    /**
     * Get the country that the city is a part of.
     * @return the country that the city is a part of.
     */
    public Country getPartOf() {
	return partOf;
    }
}
