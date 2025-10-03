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
