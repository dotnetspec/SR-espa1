
/// <reference types="cypress" />

describe('Registered User', () => {
    context('Actions', () => {
        beforeEach(() => {
            cy.visit('/')

        })

        it('successfully confirms global page header, moves to a ranking, selects Join button', () => {
            cy.contains('SportRank')
            cy.contains('Create New')
            cy.contains('Your Created Rankings:')
            cy.contains('Your Member Rankings:')
            cy.contains('Other Rankings:')

            //cy.get(':nth-child(1) > .cptr').click()
            // cy.contains('SportRank - New User - Join?')
            // cy.contains('Join')
            //cy.contains('Selected Ranking')
            //cy.get('#newUserJoinbtn').click()
            // cy.contains('Create New User')
        })



        it('successfully selects Update Profile button, confirms Update User Profile page header', () => {
            cy.get('#updateProfilebtn').click()
            cy.contains('Update User Profile')
        })

        it('successfully selects Create New Ladder button, confirms page header', () => {
            cy.get('#createnewrankingbtn').click()
            cy.contains('Create New Ladder Ranking')
        })

    })
})











