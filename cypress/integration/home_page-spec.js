describe('Registered User Home Page', () => {
    it('successfully loads', () => {
        cy.visit('/') // change URL to match your dev URL
        cy.contains('Your Created Rankings')
        cy.contains('Your Member Rankings')
        cy.contains('Other Rankings')

        //expect to have one of either created or member rankings 

    })
})