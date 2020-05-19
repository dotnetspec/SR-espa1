describe('The SportsRank Home Page', () => {
    it('successfully loads', () => {
        cy.visit('/') // change URL to match your dev URL
        cy.contains('Your Created Rankings')
    })
})