
/// <reference types="cypress" />

describe('Unregistered User Home Page', () => {
    context('Actions', () => {
        beforeEach(() => {
            cy.visit('/')

        })
        it('successfully confirms page header', () => {
            cy.contains('SportRank - New User')
            cy.contains('Register')
            cy.contains('View Rankings:')
            cy.get(':nth-child(1) > .cptr').click()
            // cy.contains('SportRank - New User - Join?')
            cy.contains('Join')
            cy.contains('Selected Ranking')
        })

        it('successfully loads a ranking', () => {
            cy.contains('SportRank - New User')
            cy.contains('Register')
            cy.contains('View Rankings:')
            cy.get(':nth-child(1) > .cptr').click()
            // cy.contains('SportRank - New User - Join?')
            cy.contains('Join')
            cy.contains('Selected Ranking')
        })
    })
})