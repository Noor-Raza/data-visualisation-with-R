# 🌳 The Evolution of San Francisco’s Trees  

This project visualizes how **San Francisco’s street trees** have expanded over the past 65 years, highlighting trends in **tree density, downtown growth, species composition, and planting activity over time**.  

It combines geospatial mapping, species-level analysis, and temporal visualization into a clear, multi-panel storytelling layout.  

<p align="center">
  <img src="assets/preview (2).gif" width="720" alt="Animated preview of SF Trees evolution">
</p>

---

## 📊 Project Overview  

Urban forestry plays a critical role in **city planning, sustainability, and climate resilience**. By analyzing the growth of trees in San Francisco, we can identify:  
- Which **neighborhoods and districts** saw the largest increases in tree coverage  
- How **tree density** changed across the city, especially downtown  
- The most **common tree species** and how their planting evolved  
- Overall **growth patterns** of the urban forest  

This project leverages **R** and pre-processed `.RData` files to produce rich visualizations that answer these questions.  

---


---

## 🔧 Tech & Skills Demonstrated  

- **Data Handling**: loading and managing `.RData` binary files  
- **Visualization**:  
  - `ggplot2` → ridgeline plots, bar charts  
  - `sf` → spatial mapping of neighborhoods  
  - `cowplot` / `patchwork` → composite layouts  
- **Geospatial Analysis**: mapping density across neighborhoods and downtown  
- **Design**: figure composition into a four-panel story  
- **Reproducibility**: project hygiene, clear structure, Git LFS for binaries  

---

## 🗺️ Figures  

The final visualization includes **four key panels**:  

1. **Citywide Map**  
   - Shows tree growth across all neighborhoods in San Francisco  
   - Tree density represented by shading, number of trees by bubble size  

2. **Downtown Map**  
   - Higher resolution view of the central district  
   - Highlights differences in density across small blocks  

3. **Species Distribution (Ridgelines)**  
   - Top species (e.g., Brisbane Box, Swamp Myrtle, Cherry Plum) plotted across decades  
   - Shows introduction and dominance patterns  

4. **Tree Growth Over Time (Bar Chart)**  
   - Number of trees planted by decade  
   - Clear growth spikes in the 1980s–2000s  

<p align="center">
  <img src="assets/5854298 (1).png" width="120" alt="Tree icon">
</p>

---
