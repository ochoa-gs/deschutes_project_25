#  Guide to Code Contribution and Version Control

This guide covers the necessary tools and the core Git/GitHub workflow for modifying code safely and collaboratively.

***

## 1. Choosing a Code Editor

This repository contains R code, so a specialized R editor is highly recommended. The two best options are:

| Code Editor | Primary Use | Why It's Recommended |
| :--- | :--- | :--- |
| **RStudio** (or RStudio Desktop) | The industry standard for R development. | Best syntax highlighting, integrated R console, **excellent Git integration**. Highly familiar environment if you use R already. |
| **VS Code** (Visual Studio Code) | A powerful, general-purpose text editor. | Excellent Git integration, highly customizable, lightweight. Requires the **"R" extension** for best R support. |

**Recommendation:** If you have R installed, **RStudio** provides the most integrated and user-friendly experience for R projects.

***

## 2. Setting Up Your Local Repository

To make and save changes, you must first create a local copy of the code on your computer.

### A. Clone the Repository

1.  **Open RStudio or VS Code.**
2.  Go to the **Source Control** area (or **New Project** in RStudio).
3.  Choose **"Clone Repository"** or **"Version Control"** and select **"Clone."**
4.  Paste the **Repository URL** (you can find this on the main GitHub page by clicking the green **< > Code** button).
5.  Select a folder on your computer to save the project.

### B. Open the Project

* **In RStudio:** Navigate to the folder you cloned and **double-click the .Rproj file**. This sets up the environment correctly (especially for the here() package).
* **In VS Code:** Go to **File > Open Folder...** and select the root project directory.

***

## 3. The Core GitHub Workflow

When you want to make changes, you should always follow these four steps: **Pull, Branch, Commit, Push.**

### Step 1:  Pull (Syncing)

Before you start any work, **pull** to make sure you have the latest version of the code that might have been pushed by your student.

* **Action:** Click the **Pull** button (usually a down arrow, or "Pull" option in the Git menu).
* **Result:** Downloads any recent changes from GitHub to your local machine.

### Step 2:  Create a Branch (Working Safely)

A **branch** is a private, temporary workspace where you can safely test changes without affecting the main code. 

[Image of Git feature branching workflow showing main branch and new feature branch]


* **Why?** If your work breaks something, the main code (main or master) remains clean.
* **Action:** In the Source Control panel, find the branch selector (usually says "main" or "master"). Click it and select **"Create New Branch."**
* **Naming:** Give it a descriptive name (e.g., fix-01-temp-bug or add-histogram-wy2020).

### Step 3:  Commit (Saving)

When you finish a specific task (e.g., fixing one variable or adding one new plot), you **commit** the changes.

1.  **Stage Files:** In the Source Control panel, stage all files you changed (the files will move from "Changes" to the "Staged Changes" list).
2.  **Write Message:** Write a clear **Commit Message** (e.g., "FEAT: Added log-transformed boxplot function to Script 04").
3.  **Commit:** Click the **Commit** button (checkmark icon). This saves a checkpoint of your work **locally** on your computer.

### Step 4:  Push (Uploading)

**Push** moves your committed changes from your local computer to the branch you created on GitHub.

* **Action:** Click the **Push** button (usually an upward arrow).
* **Result:** Your changes are now stored on GitHub in your dedicated branch.

***

## 4. Submitting Changes for Review (Pull Request)

Once you have finished your work and pushed it to your branch on GitHub, you must submit a **Pull Request (PR)** to merge it into the main project code.

1.  **Go to GitHub:** Open the project repository in your web browser.
2.  **Create PR:** GitHub will usually show a notification banner for your new branch. Click the **"Compare & Pull Request"** button.
3.  **Review:** Provide a summary of your changes.
4.  **Submit:** Click **"Create Pull Request."**
5.  **Final Merge:** Your student will review the changes, approve the request, and perform the final **Merge** into the `main` branch.