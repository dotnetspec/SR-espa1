
/// <reference types="cypress" />

describe('Registered User', () => {
    context('Actions', () => {
        beforeEach(() => {
            cy.server().route('https://api.jsonbin.io/b/5e66ec74a030db370e1b23fc/latest').as("getUsers");
            cy.server().route('https://api.jsonbin.io/b/5e4cf5f54d073155b0dca915/latest').as("getGlobalRankings");
            cy.visit('/')
            cy.get("@getUsers.2")
            cy.get("@getGlobalRankings.1")
            cy.wait("@getUsers");
            cy.wait("@getGlobalRankings");
        })

        it('successfully confirms global page header and rankings listings', () => {
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

        it('successfully moves to a ranking, selects Join button and displays Create New User', () => {
            cy.contains('SportRank')
            // cy.contains('Create New')
            // cy.contains('Your Created Rankings:')
            // cy.contains('Your Member Rankings:')
            // cy.contains('Other Rankings:')

            // cy.get(':nth-child(1) > .cptr').click()
            // cy.contains('SportRank')
            // cy.contains('Join?')
            // cy.contains('Selected Ranking')
            // cy.get('#newUserJoinbtn').click()
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











