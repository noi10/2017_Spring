package Assignment07;

import static Assignment07.Recommendation.*;

/**
 * One provision for a safari.  It includes the material identifier, material
 * description, material weight and recommendation.
 */
public class Provision {
    /** The material identifier. */
    private int materialId = 0;
    /** The material description. */
    private String description = null;
    /** The material weight. */
    private double weight = 0;
    /** The quantity of the material being used. */
    private int quantity = 1;
    /** The recommendation. */
    private Recommendation recommendation = optional;

    /**
     * Construct a provision.
     * @param materialId The material identifier.
     * @param description The material description.
     * @param weight The material weight.
     * @param quantity The quantity of the material being used.
     * @param recommendation The recommendation.
     */
    public Provision(int materialId, String description, double weight, int quantity, Recommendation recommendation) {
	this.materialId = materialId;
	this.description = description;
	this.weight = weight;
	this.quantity = quantity;
	if (recommendation == null) {
	    this.recommendation = optional;
	} else {
	    this.recommendation = recommendation;
	}
    }

    /**
     * Get the material identifier.
     * @return The identifier of the material.
     */
    public int getMaterialId() {
	return materialId;
    }

    /**
     * Get the material description.
     * @return the material description.
     */
    public String getDescription() {
	return description;
    }

    /**
     * Get the material weight.
     * @return the material weight.
     */
    public double getWeight() {
	return weight;
    }

    /**
     * Get the quantity of the material being used.
     * @return the quantity of the material being used.
     */
    public int getQuantity() {
	return quantity;
    }

    /**
     * Get the recommendation.
     * @return the recommendation.
     */
    public Recommendation getRecommendation() {
	return recommendation;
    }
}
