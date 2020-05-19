describe('Wallet Locked Error', () => {
    it('wallet error messages', () => {
        cy.visit('/') // references default URL specified in cypress.json to match your dev URL
        cy.get('#greetingHeadingStr > .s').should('have.text', `Your Ethereum  \nwallet browser\nextension is LOCKED. Please \nuse your wallet      \npassword to open it \nbefore continuing and\nrefresh the browser`)
        cy.contains('LOCKED')

        cy.get('#greetingInitStr > .s').should('have.text', `Initializing ...`)

    })
})