describe('Unregistered User Home Page', () => {
    it('successfully loads with no created or member rankings', () => {
        cy.visit('/') // change URL to match your dev URL
        cy.contains('SportRank - New User')
        cy.contains('Your Created Rankings')
        cy.contains('Create New')
        cy.contains('Your Member Rankings')
        cy.contains('Please Click On A Ranking Below To Join')
        cy.contains('Other Rankings')

    })
})