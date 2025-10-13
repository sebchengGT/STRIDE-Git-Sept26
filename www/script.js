function animateCards() {
  const cards = document.querySelectorAll(".card");
  cards.forEach(card => {
    if (!card.classList.contains("animate")) {
      card.classList.add("hidden");
      // Trigger reflow so transition applies
      void card.offsetWidth;
      card.classList.add("animate");
      card.classList.remove("hidden");
    }
  });
}

document.addEventListener("DOMContentLoaded", animateCards);

// Re-run animation when Shiny updates the UI
$(document).on('shiny:value', function() {
  animateCards();
});

// Sync card + text scaling with page zoom
function syncDashboardScale() {
  const scale = window.innerWidth / window.outerWidth;
  document.documentElement.style.setProperty('--zoom-scale', scale);
}

window.addEventListener('resize', syncDashboardScale);
window.addEventListener('load', syncDashboardScale);

// --- STRIDE Loading Screen Control ---
// --- STRIDE Loading Screen Control (Only after login) ---
$(document).ready(function () {

  // Function to show loader only in dashboard mode
  function showLoader(text) {
    if ($("body").hasClass("dashboard-bg")) {
      if (text) $("#loading-text").text(text);
      $("#loading-overlay").fadeIn(150);
    }
  }

  function hideLoader() {
    if ($("body").hasClass("dashboard-bg")) {
      setTimeout(function () {
        $("#loading-overlay").fadeOut(500);
      }, 500);
    }
  }

  // 1️⃣ Show loader on ANY nav/tab click
  // --- STRIDE Loading Screen Control (Only after login) ---
$(document).on("shiny:connected", function() {
  // Only show loader if user is already in dashboard mode
  if ($("body").hasClass("dashboard-bg")) {
    $("#loading-overlay").fadeIn(300);
  } else {
    $("#loading-overlay").hide(); // Hide loader on login page
  }
});

$(document).on("shiny:value", function() {
  // Fade out loader only when in dashboard
  if ($("body").hasClass("dashboard-bg")) {
    setTimeout(function() {
      $("#loading-overlay").fadeOut(800);
      $("body").addClass("loaded");
    }, 600);
  }
});


});




