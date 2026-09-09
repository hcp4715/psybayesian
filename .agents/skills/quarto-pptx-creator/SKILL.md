---
name: quarto-pptx-creator
description: Create educational 16:9 presentations (pptx via Revealjs) for self-study. It assists tech bloggers in analyzing diverse materials (docs, images, videos, URLs) to generate structured Quarto (.qmd) files that guide readers from theory to practice.
---

# Quarto PPTX Creator

This skill guides you through transforming raw "materials" into a logical, educational presentation for self-study.

## Workflow

Follow these steps to create a high-quality presentation:

1. **Analyze Materials**: Read and categorize all provided documents, images, videos (transcripts/descriptions), and links. Identify key theoretical concepts and practical applications.
2. **Draft Syllabus**: Create a teaching outline that moves from "Theory" (Concepts, Why, How it works) to "Practice" (Setup, Code, Exercise).
3. **Refine with User**: Present the syllabus to the user. Iterate based on their feedback until the logic and depth are approved.
4. **Generate Content**: Write the detailed educational content into a Quarto (`.qmd`) file.
   - Use the 16:9 Revealjs format (which exports to PPTX).
   - Ensure a "Self-Study" flow: clear explanations, visual aids (diagrams), and actionable steps.
5. **Final Review**: Provide the final `.qmd` file and instructions on how to render it.

## Output Guidelines

- **Format**: Always target `revealjs` with `format: pptx` compatibility in mind.
- **Ratio**: Default to 16:9.
- **Structure**:
  - **Intro**: Hook and Learning Objectives.
  - **Theory**: Core concepts explained simply.
  - **Bridge**: Moving from theory to practical context.
  - **Practice**: Step-by-step guides or code walkthroughs.
  - **Summary**: Key takeaways and next steps.

## Resources

- **Template**: See [`references/template.qmd`](references/template.qmd) for the required YAML and slide structure.
- **Example**: See [`references/example.qmd`](references/example.qmd) for a "Theory to Practice" content pattern.
