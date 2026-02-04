// Dark Mode Toggle Functionality for Learn You a Haskell

(function() {
    'use strict';
    
    // Check for saved theme preference or default to light mode
    const currentTheme = localStorage.getItem('theme') || 'light';
    
    // Apply the theme immediately to prevent flash
    if (currentTheme === 'dark') {
        document.body.classList.add('dark-mode');
    }
    
    // Create and add the toggle button
    function createToggleButton() {
        const toggleButton = document.createElement('button');
        toggleButton.className = 'dark-mode-toggle';
        toggleButton.setAttribute('aria-label', 'Toggle dark mode');
        toggleButton.setAttribute('title', 'Toggle dark mode');
        
        // Set initial icon based on current theme
        updateButtonIcon(toggleButton);
        
        // Add click handler
        toggleButton.addEventListener('click', function() {
            document.body.classList.toggle('dark-mode');
            
            // Save preference
            if (document.body.classList.contains('dark-mode')) {
                localStorage.setItem('theme', 'dark');
            } else {
                localStorage.setItem('theme', 'light');
            }
            
            // Update button icon
            updateButtonIcon(toggleButton);
        });
        
        document.body.appendChild(toggleButton);
    }
    
    function updateButtonIcon(button) {
        if (document.body.classList.contains('dark-mode')) {
            button.textContent = '☀️';
            button.setAttribute('title', 'Switch to light mode');
            button.setAttribute('aria-label', 'Switch to light mode');
        } else {
            button.textContent = '🌙';
            button.setAttribute('title', 'Switch to dark mode');
            button.setAttribute('aria-label', 'Switch to dark mode');
        }
    }
    
    // Wait for DOM to be ready
    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', createToggleButton);
    } else {
        createToggleButton();
    }
})();
